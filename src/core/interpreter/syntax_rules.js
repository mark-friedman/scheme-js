import { Cons, cons, list } from './cons.js';
import { Symbol, intern } from './symbol.js';
import { SyntaxObject, freshScope, globalScopeRegistry, internSyntax, flipScopeInExpression, identifierEquals, lookupLibraryEnv, unwrapSyntax } from './syntax_object.js';
import { globalMacroRegistry } from './macro_registry.js';
import { SPECIAL_FORMS } from './library_registry.js';

// =============================================================================
// Hygiene Support
// =============================================================================

/**
 * Counter for generating unique symbol names.
 * @type {number}
 */
let gensymCounter = 0;

/**
 * Generates a fresh, unique symbol with the given base name.
 * Used for hygienic macro expansion to rename introduced bindings.
 * @param {string} baseName - The original symbol name.
 * @returns {Symbol} A new symbol with a unique suffix.
 */
export function gensym(baseName) {
    return intern(`${baseName}#${++gensymCounter}`);
}

/**
 * Resets the gensym counter. Used for testing.
 */
export function resetGensymCounter() {
    gensymCounter = 0;
}

/**
 * Compares two identifiers using bound-identifier=? semantics.
 * Two identifiers are bound-identifier=? if they have the same name
 * and the same set of scope marks.
 * 
 * @param {Symbol|SyntaxObject} id1 - First identifier
 * @param {Symbol|SyntaxObject} id2 - Second identifier
 * @returns {boolean} True if bound-identifier=?
 */
function boundIdEquals(id1, id2) {
    // Get names
    const name1 = id1 instanceof SyntaxObject ? id1.name : (id1 instanceof Symbol ? id1.name : null);
    const name2 = id2 instanceof SyntaxObject ? id2.name : (id2 instanceof Symbol ? id2.name : null);

    if (name1 !== name2) return false;

    // Get scope sets
    const scopes1 = id1 instanceof SyntaxObject ? id1.scopes : new Set();
    const scopes2 = id2 instanceof SyntaxObject ? id2.scopes : new Set();

    // Compare scope sets
    if (scopes1.size !== scopes2.size) return false;
    for (const s of scopes1) {
        if (!scopes2.has(s)) return false;
    }
    return true;
}

/**
 * Compiles a syntax-rules specification into a transformer function.
 * 
 * @param {Array<Symbol>} literals - List of literal identifiers.
 * @param {Array} clauses - List of (pattern template) clauses.
 * @param {number|null} definingScope - Scope ID for referential transparency.
 *        Free variables in templates will be marked with this scope.
 * @param {string} ellipsisName - The ellipsis identifier (default '...')
 * @returns {Function} A transformer function (exp, useSiteEnv) -> exp.
 */
export function compileSyntaxRules(literals, clauses, definingScope = null, ellipsisName = '...', capturedEnv = null) {
    // Keep literals as objects for hygienic comparison (using bound-identifier=?)
    const literalIds = literals;

    return (exp, useSiteEnv = null) => {
        // exp is the macro call: (macro-name arg1 ...)
        // useSiteEnv is the syntactic environment at the macro invocation site

        // Generate a UNIQUE scope ID for THIS macro expansion.
        // This is the core of Dybvig-style hygiene - each expansion gets its own scope
        // so identifiers transcribed in this expansion are distinguishable from
        // identifiers transcribed in other expansions (including nested macros).
        const expansionScope = freshScope();

        for (const clause of clauses) {
            const template = clause[1];

            // Skip the macro name in the pattern matching
            // R7RS: The first element of the pattern is ignored
            let pattern = clause[0];
            let input = exp;
            if (pattern instanceof Cons && input instanceof Cons) {
                pattern = pattern.cdr;
                input = input.cdr;
            }

            // Pass useSiteEnv for free-identifier=? comparison on literals
            // Also pass expansionScope for scope-aware pattern matching
            // Pass useSiteEnv for free-identifier=? comparison on literals
            // Also pass expansionScope for scope-aware pattern matching
            // Determine Definition Environment for the literal (captured SyntacticEnv or Library Runtime Env)
            const definitionEnv = capturedEnv || (definingScope !== null ? lookupLibraryEnv(definingScope) : null);
            const bindings = matchPattern(pattern, input, literalIds, ellipsisName, useSiteEnv, expansionScope, definitionEnv);

            if (bindings) {
                // Collect pattern variables from this clause's pattern
                const patternVars = collectPatternVars(pattern, literalIds, ellipsisName);

                // Find introduced bindings in the template (symbols in binding 
                // positions that are not pattern variables or special forms)
                const introducedBindings = findIntroducedBindings(template, patternVars);

                // Generate fresh names for introduced bindings
                const renameMap = new Map();
                for (const name of introducedBindings) {
                    renameMap.set(name, gensym(name));
                }

                // Transcribe with expansionScope (not definingScope) for per-expansion hygiene
                // definingScope is still used for free variable resolution
                return transcribe(template, bindings, renameMap, expansionScope, ellipsisName, literalIds, capturedEnv);
            }
        }
        throw new Error(`No matching clause for macro use: ${exp}`);
    };
}

/**
 * Finds identifiers introduced by the template that need renaming for hygiene.
 * These are symbols in binding positions (let, lambda, letrec bindings) that:
 * - Are NOT pattern variables (would be substituted from input)
 * - Are NOT special forms (recognized by analyzer)
 * 
 * @param {*} template - The template to analyze
 * @param {Set<string>} patternVars - Set of pattern variable names
 * @returns {Set<string>} Set of names that need fresh gensyms
 */
function findIntroducedBindings(template, patternVars) {
    const introduced = new Set();
    const visited = new Set(); // Prevent infinite recursion on cyclic structures if any

    function traverse(node, inBindingPosition = false) {
        if (node instanceof Symbol) {
            if (inBindingPosition &&
                !patternVars.has(node.name) &&
                !SPECIAL_FORMS.has(node.name)) {
                introduced.add(node.name);
            }
            return;
        }

        if (!(node instanceof Cons)) return;

        // Prevent cycles just in case
        if (visited.has(node)) return;
        visited.add(node);

        const head = node.car;

        // Check for binding forms
        if (head instanceof Symbol) {
            const name = head.name;

            // (let ((var val) ...) body) or (letrec ((var val) ...) body)
            if (name === 'let' || name === 'letrec') {
                const bindings = node.cdr?.car;
                const body = node.cdr?.cdr;

                let curr = bindings;
                while (curr instanceof Cons) {
                    const pair = curr.car;
                    if (pair instanceof Cons) {
                        traverse(pair.car, true); // var
                        traverseList(pair.cdr);   // val (usually list of 1)
                    }
                    curr = curr.cdr;
                }

                traverseList(body);
                return;
            }

            // (lambda (params...) body)
            if (name === 'lambda') {
                const params = node.cdr?.car;
                const body = node.cdr?.cdr;

                traverseFormParams(params);
                traverseList(body);
                return;
            }

            // (define var val) or (define (name params...) body)
            if (name === 'define') {
                const firstArgs = node.cdr?.car;
                const rest = node.cdr?.cdr;

                if (firstArgs instanceof Symbol) {
                    // (define var val)
                    traverse(firstArgs, true); // var
                    traverseList(rest);
                } else if (firstArgs instanceof Cons) {
                    // (define (name params...) body)
                    traverse(firstArgs.car, true); // name
                    traverseFormParams(firstArgs.cdr); // params
                    traverseList(rest);
                }
                return;
            }
        }

        // Default: traverse all elements
        let curr = node;
        while (curr instanceof Cons) {
            traverse(curr.car, false);
            curr = curr.cdr;
        }
    }

    function traverseFormParams(params) {
        let curr = params;
        while (curr instanceof Cons) {
            traverse(curr.car, true);
            curr = curr.cdr;
        }
        if (curr instanceof Symbol) {
            traverse(curr, true);
        }
    }

    function traverseList(node) {
        let curr = node;
        while (curr instanceof Cons) {
            traverse(curr.car, false);
            curr = curr.cdr;
        }
    }

    traverse(template, false);
    return introduced;
}


/**
 * Matches an input expression against a pattern.
 * @param {*} pattern 
 * @param {*} input 
 * @param {Array<Symbol|SyntaxObject>} literals - List of literal identifiers
 * @param {string} ellipsisName
 * @param {SyntacticEnv} useSiteEnv - Environment at macro invocation for free-identifier=?
 * @param {number|null} expansionScope - Scope to flip on input identifiers for hygiene
 * @param {Environment|SyntacticEnv|null} definitionEnv - Environment of macro definition
 * @returns {Map<string, *> | null} Bindings map or null if failed.
 */
function matchPattern(pattern, input, literals, ellipsisName = '...', useSiteEnv = null, expansionScope = null, definitionEnv = null) {
    // 1. Variables (Symbols or SyntaxObjects)
    // SyntaxObjects can appear when patterns come from macro-expanded define-syntax
    if (pattern instanceof Symbol || pattern instanceof SyntaxObject) {
        const patName = pattern instanceof SyntaxObject ? pattern.name : pattern.name;
        if (patName === 'k' || patName === '=>') {
            console.log(`Checking Pattern: ${patName}`, pattern);
            console.log('Literals:', literals);
        }


        // Literal identifier check: use bound-identifier=? semantics
        // According to R7RS, when determining if a pattern identifier is a literal,
        // we compare it to the literals list. Both come from the macro definition,
        // so we use bound-identifier=? (comparing marks/scopes).
        const isLiteral = literals.some(lit => {
            // bound-identifier=? compares name and scope marks
            return boundIdEquals(lit, pattern);
        });

        if (patName === 'k' || patName === '=>') {
            console.log(`Checking Literal: ${patName}, isLiteral=${isLiteral}`);
        }

        if (isLiteral) {
            const patName = pattern instanceof SyntaxObject ? pattern.name : pattern.name;
            const inputName = (input instanceof Symbol) ? input.name :
                (input instanceof SyntaxObject) ? input.name : null;

            // Names must match
            if (inputName !== patName) {
                return null; // Different names - no match
            }

            // free-identifier=? semantics: if the input identifier is locally 
            // bound at the use site, it refers to that local binding, not the literal.
            // So a locally bound `=>` should NOT match the `=>` literal in cond.
            if (useSiteEnv) {
                const localBinding = useSiteEnv.lookup(input);
                if (localBinding) {
                    // Input is locally bound - it's a different identifier
                    return null;
                }
            }

            return new Map(); // Same name, not locally bound - matches literal
        }

        // Wildcard
        if (patName === '_') return new Map();

        // Pattern variable - bind input with scope marking for hygiene (Anti-Mark)
        // We flip the expansion scope on all identifiers in the matched input.
        let boundValue = input;
        if (expansionScope !== null) {
            boundValue = flipScopeInExpression(input, expansionScope);
        }
        return new Map([[pattern, boundValue]]);
    }

    // 2. Literals (Numbers, Strings, Booleans, Null)
    if (pattern === null) {
        return input === null ? new Map() : null;
    }
    if (typeof pattern !== 'object') {
        // Primitive values
        // Note: Reader produces raw primitives, but Analyzer might pass Literals?
        // No, Analyzer passes raw S-expressions to matchPattern.
        if (input === pattern) return new Map();
        return null;
    }

    // 3. Lists (Cons)
    if (pattern instanceof Cons) {
        // if (!(input instanceof Cons)) return null; // Incorrect for (x ...) matching ()

        const bindings = new Map();
        let pCurr = pattern;
        let iCurr = input;

        while (pCurr instanceof Cons) {
            const patItem = pCurr.car;

            // Check for ellipsis: look ahead
            // pCurr.cdr must be a Cons, and pCurr.cdr.car must be the ellipsis symbol
            const nextCar = pCurr.cdr instanceof Cons ? pCurr.cdr.car : null;
            const isEllipsis = nextCar &&
                ((nextCar instanceof Symbol && nextCar.name === ellipsisName) ||
                    (nextCar instanceof SyntaxObject && nextCar.name === ellipsisName));

            if (isEllipsis) {
                // Determine how many items to match Greedily (P ...)
                // We need to reserve enough items for the tail pattern T
                // Pattern is (P ... . T)
                const tailPattern = pCurr.cdr.cdr;
                const tailLen = countPairs(tailPattern);
                const inputLen = countPairs(iCurr);

                if (inputLen < tailLen) return null; // Not enough input

                // Number of items to consume for the ellipsis
                const matchCount = inputLen - tailLen;

                // Initialize bindings for all vars in patItem to []
                const varsInPat = collectPatternVars(patItem, literals, ellipsisName);
                for (const v of varsInPat) {
                    bindings.set(v, []);
                }

                // Collect matches
                for (let k = 0; k < matchCount; k++) {
                    if (!(iCurr instanceof Cons)) return null;
                    const subBindings = matchPattern(patItem, iCurr.car, literals, ellipsisName, useSiteEnv, expansionScope);
                    if (!subBindings) return null; // Failed to match one item
                    mergeBindings(bindings, subBindings, true);
                    iCurr = iCurr.cdr;
                }

                pCurr = pCurr.cdr.cdr; // Skip pat and ellipsis
            } else {
                // Normal match
                if (iCurr === null) return null; // Ran out of input

                // Handle improper input list if pattern expects more
                if (!(iCurr instanceof Cons)) return null;

                const subBindings = matchPattern(patItem, iCurr.car, literals, ellipsisName, useSiteEnv, expansionScope);
                if (!subBindings) return null;
                mergeBindings(bindings, subBindings, false);

                pCurr = pCurr.cdr;
                iCurr = iCurr.cdr;
            }
        }

        // Check tail
        if (pCurr === null) {
            if (iCurr === null) return bindings;
            return null; // Input too long
        }

        // Dotted pattern tail: (a . b)
        // pCurr is the tail (b)
        // Match tail against remaining input
        const tailBindings = matchPattern(pCurr, iCurr, literals, ellipsisName, useSiteEnv, expansionScope);
        if (!tailBindings) return null;
        mergeBindings(bindings, tailBindings, false);

        return bindings;
    }

    return null;
}

/**
 * Counts the number of Cons pairs in a list (proper or improper).
 * @param {*} list 
 * @returns {number}
 */
function countPairs(list) {
    let count = 0;
    let curr = list;
    while (curr instanceof Cons) {
        count++;
        curr = curr.cdr;
    }
    return count;
}

/**
 * Merges source bindings into target bindings.
 * @param {Map} target 
 * @param {Map} source 
 * @param {boolean} isEllipsis - If true, append values to a list.
 */
function mergeBindings(target, source, isEllipsis) {
    for (const [key, val] of source) {
        if (isEllipsis) {
            if (!target.has(key)) {
                target.set(key, []);
            }
            const list = target.get(key);
            if (!Array.isArray(list)) {
                // Should not happen if pattern is well-formed (vars don't repeat)
                throw new Error(`Pattern variable '${key}' used in both ellipsis and non-ellipsis context`);
            }
            list.push(val);
        } else {
            if (target.has(key)) {
                throw new Error(`Duplicate pattern variable '${key}'`);
            }
            target.set(key, val);
        }
    }
}

/**
 * Transcribes a template literally, without expanding ellipsis.
 * Used for escaped ellipsis (... <template>) per R7RS 4.3.2.
 * Pattern variables are still substituted, but ... is treated as a literal symbol.
 * 
 * @param {*} template - The template to transcribe literally
 * @param {Map<string, *>} bindings - Pattern variable bindings
 * @param {Map<string, Symbol>} renameMap - Map of names to their gensyms
 * @param {number|null} definingScope - Scope ID for marking free variables
 * @returns {*} Expanded expression with ... treated literally.
 */
function transcribeLiteral(template, bindings, renameMap, definingScope) {
    // Symbol or SyntaxObject: substitute pattern variables, but keep ellipsis literal
    if (template instanceof Symbol || template instanceof SyntaxObject) {
        let name = null;
        if (template instanceof Symbol) name = template.name;
        else if (template instanceof SyntaxObject) {
            name = (template.name instanceof Symbol) ? template.name.name : template.name;
        }

        // If it's the ellipsis symbol, keep it literal
        // Note: transcribeLiteral works on the escaped template content, 
        // avoiding ANY ellipsis expansion. So even if '...' appears, it's literal.
        // We only check it to avoid substituting if '...' happened to be a pattern var (unlikely for ...)
        if (name === '...') {
            return template;
        }

        // Pattern variable → substitute
        if (bindings.has(template)) {
            return bindings.get(template);
        }

        // Introduced binding → rename
        if (renameMap.has(name)) {
            return renameMap.get(name);
        }

        // Free variable with scope
        if (definingScope !== null &&
            !SPECIAL_FORMS.has(name) &&
            !globalMacroRegistry.isMacro(name)) {
            return internSyntax(name, new Set([definingScope]));
        }

        return template;
    }

    // Literals
    if (template === null || typeof template !== 'object') {
        return template;
    }

    // Lists - recurse but treat ... literally
    if (template instanceof Cons) {
        const car = transcribeLiteral(template.car, bindings, renameMap, definingScope);
        const cdr = transcribeLiteral(template.cdr, bindings, renameMap, definingScope);
        return new Cons(car, cdr);
    }

    // Vectors
    if (template instanceof Vector) {
        const elements = template.elements.map(e =>
            transcribeLiteral(e, bindings, renameMap, definingScope));
        return new Vector(elements);
    }

    return template;
}

/**
 * Transcribes a template using the bindings and rename map.
 * 
 * @param {*} template - The template to transcribe
 * @param {Map<string, *>} bindings - Pattern variable bindings
 * @param {Map<string, Symbol>} renameMap - Map of names to their gensyms
 * @param {number|null} definingScope - Scope ID for marking free variables
 * @param {string} ellipsisName - The ellipsis identifier (default '...')
 * @returns {*} Expanded expression.
 */
function transcribe(template, bindings, renameMap = new Map(), definingScope = null, ellipsisName = '...', literals = new Set(), capturedEnv = null) {
    if (template === null) return null;

    // 1. Variables (Symbols or SyntaxObjects)
    if (template instanceof Symbol || template instanceof SyntaxObject) {
        // Unwrap SyntaxObject if it wraps a Cons list (recurse on content)
        if (template instanceof SyntaxObject && template.name instanceof Cons) {
            return transcribe(template.name, bindings, renameMap, definingScope, ellipsisName, literals, capturedEnv);
        }

        const name = template instanceof SyntaxObject ? template.name : template.name;

        // Pattern variable → substitute with user input
        // Apply scope flip to the substituted value (Anti-Mark + Mark cancellation).
        if (bindings.has(template)) {
            const value = bindings.get(template);
            if (definingScope !== null) {
                return flipScopeInExpression(value, definingScope);
            }
            return value;
        }

        // Introduced binding → rename to gensym
        if (renameMap.has(name)) {
            return renameMap.get(name);
        }

        // Lexical binding from captured environment
        // If the macro was defined in a lexical scope, resolve local bindings
        if (capturedEnv && !SPECIAL_FORMS.has(name) && !globalMacroRegistry.isMacro(name)) {
            const lexicalRename = capturedEnv.lookup(template);
            if (lexicalRename) {
                // Return SyntaxObject with scope info so ScopedVariable can resolve it
                // Apply defining scope mark (Flip)
                if (template instanceof SyntaxObject) {
                    return internSyntax(lexicalRename, template.scopes).flipScope(definingScope);
                }
                // Symbol case: add scope
                return internSyntax(lexicalRename, new Set([definingScope]));
            }
        }

        // Free variable: mark with defining scope for referential transparency
        // We use flipScope (Dybvig Mark)
        if (definingScope !== null &&
            !SPECIAL_FORMS.has(name) &&
            !globalMacroRegistry.isMacro(name)) {

            if (template instanceof SyntaxObject) {
                return template.flipScope(definingScope);
            }
            return internSyntax(name, new Set([definingScope]));
        }

        // Fallback: keep as-is (primitives, special forms, macros, or no definingScope)
        // For SyntaxObject, keep it as is
        return template;
    }

    // 2. Literals
    if (typeof template !== 'object') {
        return template;
    }

    // 3. Lists (Cons)
    if (template instanceof Cons) {
        let carName = null;
        if (template.car instanceof Symbol) {
            carName = template.car.name;
        } else if (template.car instanceof SyntaxObject) {
            carName = (template.car.name instanceof Symbol) ? template.car.name.name : template.car.name;
        }

        // Check if we are missing an ellipsis due to type issues (Clean)

        // Escaped ellipsis (... <template>)
        // Per R7RS 4.3.2, (... <template>) means <template> is treated literally
        // and ellipsis within it doesn't have its special meaning
        // Note: Only the standard '...' can be used for escaping
        if (carName === '...') {
            // The cdr should be a list with one element
            if (template.cdr instanceof Cons && template.cdr.cdr === null) {
                // Return the escaped content literally
                return transcribeLiteral(template.cdr.car, bindings, renameMap, definingScope);
            }
            console.log('Malformed escaped ellipsis in transcribe:', template.toString());
            // Malformed escaped ellipsis - just return it
            // Fall through
        }

        // Check for ellipsis in template: (item <ellipsis> . rest)
        // pCurr.cdr must be a Cons, and pCurr.cdr.car must be the ellipsis symbol
        const nextCar = template.cdr instanceof Cons ? template.cdr.car : null;
        let nextCarName = null;
        if (nextCar instanceof Symbol) nextCarName = nextCar.name;
        else if (nextCar instanceof SyntaxObject) nextCarName = nextCar.name;

        // Check if next car is ellipsis AND NOT A LITERAL
        // Use identifierEquals for hygienic check against literals list
        const isEllipsis = nextCarName === ellipsisName && !literals.some(l => identifierEquals(l, nextCar));

        if (isEllipsis) {
            const item = template.car;
            const restTemplate = template.cdr.cdr;

            // Find pattern variables used in the repeating item
            const varsInItem = getPatternVars(item, bindings);

            // Separate into list bindings (ellipsis) and scalar bindings
            const listVars = varsInItem.filter(v => Array.isArray(bindings.get(v)));

            if (listVars.length === 0) {
                // Error if no pattern vars drive the ellipsis
                throw new Error("Ellipsis template must contain at least one pattern variable bound to a list");
            }



            // Check lengths of list vars
            const lengths = listVars.map(v => bindings.get(v).length);
            const len = lengths[0];
            if (!lengths.every(l => l === len)) {
                throw new Error("Ellipsis expansion: variable lengths do not match");
            }

            // Expand N times
            let expandedList = transcribe(restTemplate, bindings, renameMap, definingScope, ellipsisName, literals, capturedEnv);

            for (let i = len - 1; i >= 0; i--) {
                // Create a view of bindings for the i-th iteration
                const subBindings = new Map(bindings);
                for (const v of listVars) {
                    subBindings.set(v, bindings.get(v)[i]);
                }
                // Scalar vars remain as is in subBindings (inherited from bindings)

                const expandedItem = transcribe(item, subBindings, renameMap, definingScope, ellipsisName, literals, capturedEnv);
                expandedList = new Cons(expandedItem, expandedList);
            }

            return expandedList;
        } else {
            // Regular cons
            return new Cons(
                transcribe(template.car, bindings, renameMap, definingScope, ellipsisName, literals, capturedEnv),
                transcribe(template.cdr, bindings, renameMap, definingScope, ellipsisName, literals, capturedEnv)
            );
        }
    }

    return template;
}

/**
 * Finds all pattern variables used in a template.
 * @param {*} template 
 * @param {Map} bindings 
 * @returns {Array<string>}
 */
function getPatternVars(template, bindings) {
    const vars = new Set();
    function traverse(node) {
        // Handle both Symbol and SyntaxObject
        if (node instanceof Symbol || node instanceof SyntaxObject) {
            if (bindings.has(node)) {
                vars.add(node);
            }
        } else if (node instanceof Cons) {
            traverse(node.car);
            traverse(node.cdr);
        }
    }
    traverse(template);
    return Array.from(vars);
}

/**
 * Collects all pattern variables in a pattern.
 * @param {*} pattern 
 * @param {Array<Symbol|SyntaxObject>} literals 
 * @param {string} ellipsisName - The ellipsis identifier
 * @returns {Set<string>}
 */
function collectPatternVars(pattern, literals, ellipsisName = '...') {
    const vars = new Set();
    function traverse(node) {
        // Handle both Symbol and SyntaxObject
        if (node instanceof Symbol || node instanceof SyntaxObject) {
            const name = node instanceof SyntaxObject ? node.name : node.name;
            if (name === '_') return;
            if (name === ellipsisName) return; // Skip ellipsis
            if (literals.some(l => identifierEquals(l, node))) return;
            vars.add(node);
        } else if (node instanceof Cons) {
            traverse(node.car);
            traverse(node.cdr);
        }
    }
    traverse(pattern);
    return vars;
}

/**
 * Resolves an identifier in the lexical environment.
 * @param {Symbol|SyntaxObject} id 
 * @param {Environment|SyntacticEnv} env 
 * @returns {{type: string, frame?: Environment, name: string}|null}
 */
function resolveLexical(id, env) {
    if (!env) return null;
    const name = id instanceof SyntaxObject ? id.name : id.name;

    // 1. Check for SyntacticEnv (Compile Time)
    // SyntacticEnv (from neighbor files) has 'bindings' Array and 'lookup' method, but no 'findEnv'
    if (env.bindings && Array.isArray(env.bindings)) {
        const renamed = env.lookup(id);
        if (renamed) {
            return { type: 'syntactic', name: renamed };
        }
        return null;
    }

    // 2. Check for Environment (Runtime)
    // Environment has 'findEnv'
    if (typeof env.findEnv === 'function') {
        const frame = env.findEnv(name);
        if (frame) return { type: 'runtime', frame, name };
        return null; // Unbound in runtime env
    }

    // Fallback or unknown env type
    return null;
}

/**
 * Resolves an identifier to its binding (Lexical or Global).
 * @param {Symbol|SyntaxObject} id 
 * @param {Environment} env 
 * @returns {{type: string, frame?: Environment, name?: string, binding?: any}|null}
 */
function resolveIdentifier(id, env) {
    // 1. Try Lexical Env
    const lex = resolveLexical(id, env);
    if (lex) return { type: 'lexical', ...lex };

    // 2. Try Global Registry (Scopes)
    // GlobalScopeRegistry resolution relies on Identifier Scopes.
    // syntax_object.js exports globalScopeRegistry.
    // Note: globalScopeRegistry uses SyntaxObject identity/scopes lookup
    const globalBinding = globalScopeRegistry.resolve(id);
    if (globalBinding) {
        return { type: 'global', binding: globalBinding };
    }

    return null; // Unbound
}

/**
 * Checks if two identifiers are free-identifier=?
 * (They resolve to the same binding in their respective environments)
 * @param {Symbol|SyntaxObject} id1 
 * @param {Symbol|SyntaxObject} id2 
 * @param {Environment} env1 - Definition Environment (for id1)
 * @param {Environment} env2 - Use Site Environment (for id2)
 * @returns {boolean}
 */
function freeIdentifierEquals(id1, id2, env1, env2) {
    // 1. Resolve bindings
    const b1 = resolveIdentifier(id1, env1);
    const b2 = resolveIdentifier(id2, env2);

    // 2. Compare locations
    if (b1 && b2) {
        // Both bound. Compare locations.
        if (b1.type === 'lexical' && b2.type === 'lexical') {
            return b1.frame === b2.frame && b1.name === b2.name;
        }
        if (b1.type === 'global' && b2.type === 'global') {
            // Compare resolved global bindings from Registry
            // If binding is an object (GlobalRef), reference equality matters.
            // If primitive value, value equality.
            // GlobalRef instances are created per define? 
            // Yes, registerBindingWithCurrentScopes creates unique GlobalRef.
            // So reference equality should work if they refer to SAME definition.
            return b1.binding === b2.binding;
        }
        // Mixed types (one lexical, one global) => Not equal
        return false;
    }

    // 3. One or both unbound
    if (!b1 && !b2) {
        // Both unbound. Compare names.
        const n1 = id1 instanceof SyntaxObject ? id1.name : id1.name;
        const n2 = id2 instanceof SyntaxObject ? id2.name : id2.name;
        return n1 === n2;
    }

    // One bound, one unbound => Not equal
    return false;
}
