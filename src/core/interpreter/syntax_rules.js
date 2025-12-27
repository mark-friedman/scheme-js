import { Cons, cons, list } from './cons.js';
import { Symbol, intern } from './symbol.js';
import { SyntaxObject, freshScope, globalScopeRegistry } from './syntax_object.js';
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
    const literalNames = new Set(literals.map(l => l.name));

    return (exp, useSiteEnv = null) => {
        // exp is the macro call: (macro-name arg1 ...)
        // useSiteEnv is the syntactic environment at the macro invocation site
        // We match against the whole expression.

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
            const bindings = matchPattern(pattern, input, literalNames, ellipsisName, useSiteEnv);

            if (bindings) {
                // Collect pattern variables from this clause's pattern
                const patternVars = collectPatternVars(pattern, literalNames, ellipsisName);

                // Find introduced bindings in the template (symbols in binding 
                // positions that are not pattern variables or special forms)
                const introducedBindings = findIntroducedBindings(template, patternVars);

                // Generate fresh names for introduced bindings
                const renameMap = new Map();
                for (const name of introducedBindings) {
                    renameMap.set(name, gensym(name));
                }

                // Transcribe with the rename map for hygiene and definingScope
                // for referential transparency
                return transcribe(template, bindings, renameMap, definingScope, ellipsisName, literalNames, capturedEnv);
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
 * @param {Set<string>} literals 
 * @param {string} ellipsisName
 * @param {SyntacticEnv} useSiteEnv - Environment at macro invocation for free-identifier=?
 * @returns {Map<string, *> | null} Bindings map or null if failed.
 */
function matchPattern(pattern, input, literals, ellipsisName = '...', useSiteEnv = null) {
    // 1. Variables (Symbols or SyntaxObjects)
    // SyntaxObjects can appear when patterns come from macro-expanded define-syntax
    if (pattern instanceof Symbol || pattern instanceof SyntaxObject) {
        const patName = pattern instanceof SyntaxObject ? pattern.name : pattern.name;

        // Literal identifier - use free-identifier=? semantics
        if (literals.has(patName)) {
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

        // Pattern variable
        return new Map([[patName, input]]);
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
                    const subBindings = matchPattern(patItem, iCurr.car, literals, ellipsisName, useSiteEnv);
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

                const subBindings = matchPattern(patItem, iCurr.car, literals, ellipsisName, useSiteEnv);
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
        const tailBindings = matchPattern(pCurr, iCurr, literals, ellipsisName, useSiteEnv);
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
        const name = template instanceof SyntaxObject ? template.name : template.name;

        // If it's the ellipsis symbol, keep it literal
        // Note: transcribeLiteral works on the escaped template content, 
        // avoiding ANY ellipsis expansion. So even if '...' appears, it's literal.
        // We only check it to avoid substituting if '...' happened to be a pattern var (unlikely for ...)
        if (name === '...') {
            return template;
        }

        // Pattern variable → substitute
        if (bindings.has(name)) {
            return bindings.get(name);
        }

        // Introduced binding → rename
        if (renameMap.has(name)) {
            return renameMap.get(name);
        }

        // Free variable with scope
        if (definingScope !== null &&
            !SPECIAL_FORMS.has(name) &&
            !globalMacroRegistry.isMacro(name)) {
            return new SyntaxObject(name, new Set([definingScope]));
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
        if (bindings.has(name)) {
            return bindings.get(name);
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
                if (template instanceof SyntaxObject) {
                    return new SyntaxObject(lexicalRename, template.scopes);
                }
                return intern(lexicalRename);
            }
        }

        // Free variable: mark with defining scope for referential transparency
        // Special forms are NOT marked - they're keywords recognized by the analyzer
        // Macros are NOT marked - they're expanded at analysis time, not runtime values
        if (definingScope !== null &&
            !SPECIAL_FORMS.has(name) &&
            !globalMacroRegistry.isMacro(name)) {
            // Return a SyntaxObject with the defining scope mark
            // If already a SyntaxObject, add the scope
            if (template instanceof SyntaxObject) {
                return template.addScope(definingScope);
            }
            return new SyntaxObject(name, new Set([definingScope]));
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
        const carName = (template.car instanceof Symbol) ? template.car.name :
            (template.car instanceof SyntaxObject) ? template.car.name : null;

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
        const isEllipsis = nextCarName === ellipsisName && !literals.has(nextCarName);

        if (isEllipsis) {
            const item = template.car;
            const restTemplate = template.cdr.cdr;

            // Find pattern variables used in the repeating item
            const varsInItem = getPatternVars(item, bindings);

            // Separate into list bindings (ellipsis) and scalar bindings
            const listVars = varsInItem.filter(v => Array.isArray(bindings.get(v)));

            if (listVars.length === 0) {
                // If no list vars, it might be a literal repetition or error.
                // For now, we require at least one list var to determine length.
                // (Or we could support literal repetition if we knew the length from somewhere else?)
                // But standard syntax-rules usually implies repetition is driven by pattern vars.
                // If there are NO list vars, we can't determine how many times to repeat.
                // Unless we allow 0 times? Or infinite?
                // Let's assume it's an error for now if no driving variables are present.
                // BUT wait, what if the template is just (literal ...)?
                // That's usually invalid unless there's a pattern var.
                console.error('Ellipsis template error:', {
                    template: template.toString(),
                    item: item.toString(),
                    varsInItem,
                    bindings: Array.from(bindings.keys())
                });
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
            const name = node instanceof SyntaxObject ? node.name : node.name;
            if (bindings.has(name)) {
                vars.add(name);
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
 * @param {Set<string>} literals 
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
            if (literals.has(name)) return;
            vars.add(name);
        } else if (node instanceof Cons) {
            traverse(node.car);
            traverse(node.cdr);
        }
    }
    traverse(pattern);
    return vars;
}
