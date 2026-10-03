import { Cons, cons, list, toArray } from './cons.js';
import { Symbol, intern } from './symbol.js';
import { SyntaxObject, globalScopeRegistry, internSyntax, flipScopeInExpression, identifierEquals, unwrapSyntax, libraryScopeOf, keywordName } from './syntax_object.js';
import { globalContext } from './context.js';
import { globalMacroRegistry } from './macro_registry.js';
import { SPECIAL_FORMS } from './library_registry.js';
import { getIdentifierName, isEllipsisIdentifier } from './identifier_utils.js';
import { SchemeSyntaxError } from './errors.js';

// =============================================================================
// Hygiene Support
// =============================================================================

// Pure Marks Hygiene: Instead of renaming introduced bindings with gensyms,
// we mark all template identifiers with a fresh expansion scope. Two identifiers
// with the same name but different scope sets are distinct bindings.


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
    // Get names using shared utility
    const name1 = getIdentifierName(id1);
    const name2 = getIdentifierName(id2);

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
 * @param {number|null} definingScope - Scope ID where the macro was defined.
 *        Used for looking up the definition environment.
 * @param {string} ellipsisName - The ellipsis identifier (default '...')
 * @param {Environment|null} capturedEnv - Lexical environment at macro definition
 * @returns {Function} A transformer function (exp, useSiteEnv) -> exp.
 */
export function compileSyntaxRules(literals, clauses, definingScope = null, ellipsisName = '...', capturedEnv = null) {
    // Keep literals as objects for hygienic comparison (using bound-identifier=?)
    const literalIds = literals;

    // A macro defined in a library marks what its templates introduce with
    // the library's scope, so that their references find the library's
    // bindings (see `libraryBindingEnv` in syntax_object.js).
    const libraryEnv = definingScope !== null ? globalContext.lookupLibraryEnv(definingScope) ?? null : null;
    const libraryScope = libraryEnv !== null ? definingScope : null;
    // The libraries the macro's expansions name by scope: its own, and those
    // whose macros' expansions defined it, whose scopes its identifiers
    // carry. The macro may outlive their registry -- it is defined by name
    // for the process -- so it holds them itself.
    const libraries = librariesNamedIn([literals, clauses], libraryEnv === null ? [] : [libraryEnv]);

    return (exp, useSiteEnv = null) => {
        // exp is the macro call: (macro-name arg1 ...)
        // useSiteEnv is the syntactic environment at the macro invocation site

        // The libraries this expansion names by scope are found by scope
        // while it is analyzed, wherever the macro is used: their registry
        // may have gone, and taken their entries with it.
        for (const env of libraries) {
            if (globalContext.lookupLibraryEnv(env.libraryScope) === undefined) {
                globalContext.registerLibraryScope(env.libraryScope, env);
            }
        }

        // Generate a UNIQUE scope ID for THIS macro expansion.
        // This is the core of Dybvig-style hygiene - each expansion gets its own scope
        // so identifiers transcribed in this expansion are distinguishable from
        // identifiers transcribed in other expansions (including nested macros).
        const expansionScope = globalContext.freshScope();

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
            // Determine definition environment for literal comparison
            const definitionEnv = capturedEnv || libraryEnv;
            const bindings = matchPattern(pattern, input, literalIds, ellipsisName, useSiteEnv, expansionScope, definitionEnv, definingScope);

            if (bindings) {
                // Pure marks hygiene: no renaming needed.
                // All identifiers will be marked with expansionScope during transcription,
                // making introduced bindings distinguishable from user bindings.
                return transcribe(template, bindings, expansionScope, ellipsisName, literalIds, capturedEnv, libraryScope);
            }
        }
        // Extract macro name for better error message
        const macroName = exp instanceof Cons && (exp.car instanceof Symbol || exp.car instanceof SyntaxObject)
            ? (exp.car instanceof Symbol ? exp.car.name : exp.car.name)
            : 'unknown';
        throw new SchemeSyntaxError(`No matching clause for macro '${macroName}'`, exp, macroName);
    };
}

/**
 * Adds to `libraries` the environment of each library whose scope an
 * identifier in `datum` carries.
 * @param {*} datum - Syntax: pairs, vectors and arrays of them, identifiers.
 * @param {Array<Environment>} libraries - The environments found so far.
 * @returns {Array<Environment>} `libraries`.
 */
function librariesNamedIn(datum, libraries) {
    const seen = new Set();
    const visit = (node) => {
        while (node instanceof Cons) {
            if (seen.has(node)) return;
            seen.add(node);
            visit(node.car);
            node = node.cdr;
        }
        if (Array.isArray(node)) {
            if (seen.has(node)) return;
            seen.add(node);
            node.forEach(visit);
        } else if (node instanceof SyntaxObject) {
            for (const scope of node.scopes) {
                const env = globalContext.lookupLibraryEnv(scope);
                if (env !== undefined && !libraries.includes(env)) libraries.push(env);
            }
        }
    };
    visit(datum);
    return libraries;
}

// Note: findIntroducedBindings was removed as part of the pure marks refactor.
// Introduced bindings are now distinguished by scope marks, not renaming.


/**
 * Matches an input expression against a pattern.
 * @param {*} pattern 
 * @param {*} input 
 * @param {Array<Symbol|SyntaxObject>} literals - List of literal identifiers
 * @param {string} ellipsisName
 * @param {SyntacticEnv} useSiteEnv - Environment at macro invocation for free-identifier=?
 * @param {number|null} expansionScope - Scope to flip on input identifiers for hygiene
 * @param {Environment|SyntacticEnv|null} definitionEnv - Environment of macro definition
 * @param {number|null} literalScope - Where the macro was defined, which is
 *        where its literals name the keywords they do
 * @returns {Map<string, *> | null} Bindings map or null if failed.
 */
function matchPattern(pattern, input, literals, ellipsisName = '...', useSiteEnv = null, expansionScope = null, definitionEnv = null, literalScope = null) {
    // 1. Variables (Symbols or SyntaxObjects)
    // SyntaxObjects can appear when patterns come from macro-expanded define-syntax
    if (pattern instanceof Symbol || pattern instanceof SyntaxObject) {
        const patName = pattern instanceof SyntaxObject ? pattern.name : pattern.name;

        // Literal identifier check: use bound-identifier=? semantics
        // According to R7RS, when determining if a pattern identifier is a literal,
        // we compare it to the literals list. Both come from the macro definition,
        // so we use bound-identifier=? (comparing marks/scopes).
        const isLiteral = literals.some(lit => {
            // bound-identifier=? compares name and scope marks
            return boundIdEquals(lit, pattern);
        });

        if (isLiteral) {
            // The input must name what the literal does: the same name, or
            // the same keyword imported under different names where each is.
            if (!(input instanceof Symbol || input instanceof SyntaxObject)) {
                return null;
            }
            if (keywordName(input) !== keywordName(pattern, literalScope)) {
                return null;
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

    // 3. Vectors: #(P ...) matches a vector whose elements match, just as
    // (P ...) matches a list of them, ellipses included (R7RS 4.3.2).
    if (Array.isArray(pattern)) {
        if (!Array.isArray(input)) return null;
        return matchPattern(list(...pattern), list(...input), literals, ellipsisName, useSiteEnv, expansionScope, definitionEnv, literalScope);
    }

    // 4. Lists (Cons)
    if (pattern instanceof Cons) {
        // if (!(input instanceof Cons)) return null; // Incorrect for (x ...) matching ()

        const bindings = new Map();
        let pCurr = pattern;
        let iCurr = input;

        while (pCurr instanceof Cons) {
            const patItem = pCurr.car;

            // Check for ellipsis: look ahead using shared utility
            const nextCar = pCurr.cdr instanceof Cons ? pCurr.cdr.car : null;
            const isEllipsis = nextCar && isEllipsisIdentifier(nextCar, ellipsisName);

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
                    const subBindings = matchPattern(patItem, iCurr.car, literals, ellipsisName, useSiteEnv, expansionScope, definitionEnv, literalScope);
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

                const subBindings = matchPattern(patItem, iCurr.car, literals, ellipsisName, useSiteEnv, expansionScope, definitionEnv, literalScope);
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
        const tailBindings = matchPattern(pCurr, iCurr, literals, ellipsisName, useSiteEnv, expansionScope, definitionEnv, literalScope);
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
                throw new SchemeSyntaxError(`Pattern variable '${key}' used in both ellipsis and non-ellipsis context`, null, 'syntax-rules');
            }
            list.push(val);
        } else {
            if (target.has(key)) {
                throw new SchemeSyntaxError(`Duplicate pattern variable '${key}'`, null, 'syntax-rules');
            }
            target.set(key, val);
        }
    }
}

/**
 * Marks an identifier a template introduces, rather than one a pattern
 * variable substitutes: with the expansion's scope, which keeps it apart from
 * the user's identifiers of the same name, and, if a library defined the
 * macro, with the library's scope, which is how a reference it makes finds
 * the library's binding of its name.
 *
 * An identifier carrying a library's scope already keeps it alone: it was
 * written in that library, by a macro of its that wrote this macro's template,
 * and refers into that library, not this one.
 *
 * @param {Symbol|SyntaxObject} id - The template's identifier.
 * @param {number} expansionScope - This expansion's scope.
 * @param {number|null} libraryScope - The defining library's scope, or null.
 * @returns {SyntaxObject|Symbol} The marked identifier.
 */
function markIntroduced(id, expansionScope, libraryScope) {
    const inLibrary = libraryScope !== null && libraryScopeOf(id) === null;
    if (id instanceof SyntaxObject) {
        return (inLibrary ? id.addScope(libraryScope) : id).flipScope(expansionScope);
    }
    return internSyntax(id.name, new Set(inLibrary ? [libraryScope, expansionScope] : [expansionScope]));
}

/**
 * Transcribes a template literally (for escaped ellipsis handling).
 * Marks all identifiers with the expansion scope.
 * 
 * @param {*} template - The template to transcribe
 * @param {Map} bindings - Pattern variable bindings
 * @param {number|null} expansionScope - Scope ID for marking free variables
 * @param {number|null} [libraryScope] - The scope of the library that defined
 *        the macro, or null if no library did
 * @returns {*} The transcribed literal
 */
function transcribeLiteral(template, bindings, expansionScope, libraryScope = null) {
    // Symbol or SyntaxObject: substitute pattern variables, keep others with scope mark
    if (template instanceof Symbol || template instanceof SyntaxObject) {
        let name = null;
        if (template instanceof Symbol) name = template.name;
        else if (template instanceof SyntaxObject) {
            name = (template.name instanceof Symbol) ? template.name.name : template.name;
        }

        // If it's the ellipsis symbol, keep it literal
        if (name === '...') {
            return template;
        }

        // Pattern variable → substitute
        if (bindings.has(template)) {
            return bindings.get(template);
        }

        // All other identifiers: mark with expansion scope (pure marks hygiene)
        if (expansionScope !== null) {
            return markIntroduced(template, expansionScope, libraryScope);
        }

        return template;
    }

    // Literals
    if (template === null || typeof template !== 'object') {
        return template;
    }

    // Vectors - their elements, likewise
    if (Array.isArray(template)) {
        return template.map((element) => transcribeLiteral(element, bindings, expansionScope, libraryScope));
    }

    // Lists - recurse but treat ... literally
    if (template instanceof Cons) {
        const car = transcribeLiteral(template.car, bindings, expansionScope, libraryScope);
        const cdr = transcribeLiteral(template.cdr, bindings, expansionScope, libraryScope);
        return new Cons(car, cdr);
    }

    return template;
}

/**
 * Transcribes a template using the bindings.
 * All template identifiers are marked with expansionScope for pure marks hygiene.
 * 
 * @param {*} template - The template to transcribe
 * @param {Map<string, *>} bindings - Pattern variable bindings
 * @param {number|null} expansionScope - Scope ID for marking identifiers (per-expansion)
 * @param {string} ellipsisName - The ellipsis identifier (default '...')
 * @param {Array} literals - List of literal identifiers
 * @param {Environment|null} capturedEnv - Captured lexical environment
 * @param {number|null} libraryScope - The scope of the library that defined
 *        the macro, or null if no library did
 * @returns {*} Expanded expression.
 */
function transcribe(template, bindings, expansionScope = null, ellipsisName = '...', literals = new Set(), capturedEnv = null, libraryScope = null) {
    if (template === null) return null;

    // 1. Variables (Symbols or SyntaxObjects)
    if (template instanceof Symbol || template instanceof SyntaxObject) {
        // Unwrap SyntaxObject if it wraps a Cons list (recurse on content)
        if (template instanceof SyntaxObject && template.name instanceof Cons) {
            return transcribe(template.name, bindings, expansionScope, ellipsisName, literals, capturedEnv, libraryScope);
        }

        const name = template instanceof SyntaxObject ? template.name : template.name;

        // Pattern variable → substitute with user input
        // Apply scope flip to the substituted value (Anti-Mark + Mark cancellation).
        if (bindings.has(template)) {
            const value = bindings.get(template);
            if (expansionScope !== null) {
                return flipScopeInExpression(value, expansionScope);
            }
            return value;
        }

        // Lexical binding from captured environment
        // If the macro was defined in a lexical scope, resolve local bindings
        if (capturedEnv && !SPECIAL_FORMS.has(name) && !globalMacroRegistry.isMacro(name)) {
            const lexicalRename = capturedEnv.lookup(template);
            if (lexicalRename) {
                // Return SyntaxObject with scope info so ScopedVariable can resolve it
                if (template instanceof SyntaxObject) {
                    return internSyntax(lexicalRename, template.scopes).flipScope(expansionScope);
                }
                return internSyntax(lexicalRename, new Set([expansionScope]));
            }
        }

        // All other identifiers (free variables, introduced bindings): mark with expansion scope
        if (expansionScope !== null) {
            return markIntroduced(template, expansionScope, libraryScope);
        }

        // Fallback: keep as-is (primitives, special forms, macros, or no expansionScope)
        return template;
    }

    // 2. Literals
    if (typeof template !== 'object') {
        return template;
    }

    // 3. Vectors: built from their elements as the list of them would be,
    // ellipses included.
    if (Array.isArray(template)) {
        return toArray(transcribe(list(...template), bindings, expansionScope, ellipsisName, literals, capturedEnv, libraryScope));
    }

    // 4. Lists (Cons)
    if (template instanceof Cons) {
        let carName = null;
        if (template.car instanceof Symbol) {
            carName = template.car.name;
        } else if (template.car instanceof SyntaxObject) {
            carName = (template.car.name instanceof Symbol) ? template.car.name.name : template.car.name;
        }

        // Escaped ellipsis (<ellipsis> <template>)
        // Per R7RS 4.3.2, (<ellipsis> <template>) means <template> is treated literally
        // This applies to the custom ellipsis name if one was specified
        if (carName === ellipsisName) {
            if (template.cdr instanceof Cons && template.cdr.cdr === null) {
                return transcribeLiteral(template.cdr.car, bindings, expansionScope, libraryScope);
            }
        }

        // Check for ellipsis in template: (item <ellipsis> . rest)
        const nextCar = template.cdr instanceof Cons ? template.cdr.car : null;
        const isEllipsis = nextCar && isEllipsisIdentifier(nextCar, ellipsisName, Array.from(literals));

        if (isEllipsis) {
            const item = template.car;
            const restTemplate = template.cdr.cdr;

            // Find pattern variables used in the repeating item
            const varsInItem = getPatternVars(item, bindings);

            // Separate into list bindings (ellipsis) and scalar bindings
            const listVars = varsInItem.filter(v => Array.isArray(bindings.get(v)));

            if (listVars.length === 0) {
                throw new SchemeSyntaxError('Ellipsis template must contain at least one pattern variable bound to a list', null, 'syntax-rules');
            }

            // Check lengths of list vars
            const lengths = listVars.map(v => bindings.get(v).length);
            const len = lengths[0];
            if (!lengths.every(l => l === len)) {
                throw new SchemeSyntaxError('Ellipsis expansion: variable lengths do not match', null, 'syntax-rules');
            }

            // Expand N times
            let expandedList = transcribe(restTemplate, bindings, expansionScope, ellipsisName, literals, capturedEnv, libraryScope);

            for (let i = len - 1; i >= 0; i--) {
                const subBindings = new Map(bindings);
                for (const v of listVars) {
                    subBindings.set(v, bindings.get(v)[i]);
                }
                const expandedItem = transcribe(item, subBindings, expansionScope, ellipsisName, literals, capturedEnv, libraryScope);
                expandedList = new Cons(expandedItem, expandedList);
            }

            return expandedList;
        } else {
            // Regular cons
            return new Cons(
                transcribe(template.car, bindings, expansionScope, ellipsisName, literals, capturedEnv, libraryScope),
                transcribe(template.cdr, bindings, expansionScope, ellipsisName, literals, capturedEnv, libraryScope)
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
        } else if (Array.isArray(node)) {
            node.forEach(traverse);
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
        } else if (Array.isArray(node)) {
            node.forEach(traverse);
        }
    }
    traverse(pattern);
    return vars;
}
