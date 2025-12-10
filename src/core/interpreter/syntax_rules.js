import { Cons, cons, list } from './cons.js';
import { Symbol } from './symbol.js';

/**
 * Compiles a syntax-rules specification into a transformer function.
 * @param {Array<Symbol>} literals - List of literal identifiers.
 * @param {Array} clauses - List of (pattern template) clauses.
 * @returns {Function} A transformer function (exp) -> exp.
 */
export function compileSyntaxRules(literals, clauses) {
    const literalNames = new Set(literals.map(l => l.name));

    return (exp) => {
        // exp is the macro call: (macro-name arg1 ...)
        // We match against the whole expression.

        for (const clause of clauses) {
            const pattern = clause[0];
            const template = clause[1];

            // Match against the pattern
            const bindings = matchPattern(pattern, exp, literalNames);

            if (bindings) {
                // If match successful, transcribe the template
                return transcribe(template, bindings);
            }
        }
        throw new Error(`No matching clause for macro use: ${exp}`);
    };
}

/**
 * Matches an input expression against a pattern.
 * @param {*} pattern 
 * @param {*} input 
 * @param {Set<string>} literals 
 * @returns {Map<string, *> | null} Bindings map or null if failed.
 */
function matchPattern(pattern, input, literals) {
    // 1. Variables (Symbols)
    if (pattern instanceof Symbol) {
        // Wildcard
        if (pattern.name === '_') return new Map();

        // Literal identifier
        if (literals.has(pattern.name)) {
            if (input instanceof Symbol && input.name === pattern.name) {
                return new Map();
            }
            return null; // Mismatch
        }

        // Pattern variable
        return new Map([[pattern.name, input]]);
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
            // pCurr.cdr must be a Cons, and pCurr.cdr.car must be '...'
            const isEllipsis = (pCurr.cdr instanceof Cons) &&
                (pCurr.cdr.car instanceof Symbol) &&
                (pCurr.cdr.car.name === '...');

            if (isEllipsis) {
                // Greedy match for the rest of the input (tail ellipsis)

                // Initialize bindings for all vars in patItem to []
                const varsInPat = collectPatternVars(patItem, literals);
                for (const v of varsInPat) {
                    bindings.set(v, []);
                }

                // Collect all matches
                while (iCurr instanceof Cons) {
                    const subBindings = matchPattern(patItem, iCurr.car, literals);
                    if (!subBindings) return null; // Failed to match one item
                    mergeBindings(bindings, subBindings, true);
                    iCurr = iCurr.cdr;
                }

                // If iCurr is not null, it means input was improper list or longer?
                // But we consumed all Cons. If iCurr is not null, it's the dotted tail.
                // But pattern has ellipsis, so it expects list.
                // Standard syntax-rules: (x ...) matches proper list.
                if (iCurr !== null) return null;

                pCurr = pCurr.cdr.cdr; // Skip pat and ...
            } else {
                // Normal match
                if (iCurr === null) return null; // Ran out of input

                // Handle improper input list if pattern expects more
                if (!(iCurr instanceof Cons)) return null;

                const subBindings = matchPattern(patItem, iCurr.car, literals);
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
        const tailBindings = matchPattern(pCurr, iCurr, literals);
        if (!tailBindings) return null;
        mergeBindings(bindings, tailBindings, false);

        return bindings;
    }

    return null;
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
 * Transcribes a template using the bindings.
 * @param {*} template 
 * @param {Map<string, *>} bindings 
 * @returns {*} Expanded expression.
 */
function transcribe(template, bindings) {
    // 1. Variables (Symbols)
    if (template instanceof Symbol) {
        if (bindings.has(template.name)) {
            return bindings.get(template.name);
        }
        return template;
    }

    // 2. Literals
    if (template === null || typeof template !== 'object') {
        return template;
    }

    // 3. Lists (Cons)
    if (template instanceof Cons) {
        // Check for ellipsis in template
        // (item ... . rest)
        const isEllipsis = (template.cdr instanceof Cons) &&
            (template.cdr.car instanceof Symbol) &&
            (template.cdr.car.name === '...');

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
                throw new Error("Ellipsis template must contain at least one pattern variable bound to a list");
            }

            // Check lengths of list vars
            const lengths = listVars.map(v => bindings.get(v).length);
            const len = lengths[0];
            if (!lengths.every(l => l === len)) {
                throw new Error("Ellipsis expansion: variable lengths do not match");
            }

            // Expand N times
            let expandedList = transcribe(restTemplate, bindings);

            for (let i = len - 1; i >= 0; i--) {
                // Create a view of bindings for the i-th iteration
                const subBindings = new Map(bindings);
                for (const v of listVars) {
                    subBindings.set(v, bindings.get(v)[i]);
                }
                // Scalar vars remain as is in subBindings (inherited from bindings)

                const expandedItem = transcribe(item, subBindings);
                expandedList = new Cons(expandedItem, expandedList);
            }

            return expandedList;
        } else {
            // Regular cons
            return new Cons(
                transcribe(template.car, bindings),
                transcribe(template.cdr, bindings)
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
        if (node instanceof Symbol) {
            if (bindings.has(node.name)) {
                vars.add(node.name);
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
 * @returns {Set<string>}
 */
function collectPatternVars(pattern, literals) {
    const vars = new Set();
    function traverse(node) {
        if (node instanceof Symbol) {
            if (node.name === '_') return;
            if (literals.has(node.name)) return;
            vars.add(node.name);
        } else if (node instanceof Cons) {
            traverse(node.car);
            traverse(node.cdr);
        }
    }
    traverse(pattern);
    return vars;
}
