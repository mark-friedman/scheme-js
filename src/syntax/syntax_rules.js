import { Variable, Literal } from './ast.js';
import { globalMacroRegistry } from './macro_registry.js';

/**
 * Compiles a syntax-rules specification into a transformer function.
 * @param {Array} literals - List of literal identifiers (Variables).
 * @param {Array} clauses - List of (pattern template) clauses.
 * @returns {Function} A transformer function (exp) -> exp.
 */
export function compileSyntaxRules(literals, clauses) {
    const literalNames = new Set(literals.map(l => l.name));

    return (exp) => {
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
    // 1. Variables
    if (pattern instanceof Variable) {
        // Wildcard
        if (pattern.name === '_') return new Map();

        // Literal identifier
        if (literals.has(pattern.name)) {
            if (input instanceof Variable && input.name === pattern.name) {
                return new Map();
            }
            return null; // Mismatch
        }

        // Pattern variable
        return new Map([[pattern.name, input]]);
    }

    // 2. Literals (Numbers, Strings, Booleans)
    if (pattern instanceof Literal) {
        if (input instanceof Literal && input.value === pattern.value) {
            return new Map();
        }
        return null;
    }

    // 3. Lists
    if (Array.isArray(pattern)) {
        if (!Array.isArray(input)) return null;

        const bindings = new Map();
        let pIdx = 0;
        let iIdx = 0;

        while (pIdx < pattern.length) {
            const patItem = pattern[pIdx];

            // Check for ellipsis: look ahead
            const isEllipsis = (pIdx + 1 < pattern.length) &&
                (pattern[pIdx + 1] instanceof Variable) &&
                (pattern[pIdx + 1].name === '...');

            if (isEllipsis) {
                // Greedy match for the rest of the input (tail ellipsis)
                // TODO: Support non-tail ellipsis (requires looking ahead in pattern)

                // For now, assume ellipsis consumes everything until the end 
                // OR until we implement better lookahead. 
                // Standard syntax-rules allows (x ... y), but (x ...) is most common.

                // Collect all matches
                while (iIdx < input.length) {
                    const subBindings = matchPattern(patItem, input[iIdx], literals);
                    if (!subBindings) return null; // Failed to match one item
                    mergeBindings(bindings, subBindings, true);
                    iIdx++;
                }

                pIdx += 2; // Skip pat and ...
            } else {
                // Normal match
                if (iIdx >= input.length) return null; // Ran out of input

                const subBindings = matchPattern(patItem, input[iIdx], literals);
                if (!subBindings) return null;
                mergeBindings(bindings, subBindings, false);

                pIdx++;
                iIdx++;
            }
        }

        if (iIdx < input.length) return null; // Leftover input
        return bindings;
    }

    // 4. Other (shouldn't happen in AST usually, maybe null?)
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
    // 1. Variables
    if (template instanceof Variable) {
        if (bindings.has(template.name)) {
            return bindings.get(template.name);
        }
        return template;
    }

    // 2. Literals
    if (template instanceof Literal) {
        return template;
    }

    // 3. Lists
    if (Array.isArray(template)) {
        const result = [];
        let tIdx = 0;

        while (tIdx < template.length) {
            const item = template[tIdx];

            // Check for ellipsis
            const isEllipsis = (tIdx + 1 < template.length) &&
                (template[tIdx + 1] instanceof Variable) &&
                (template[tIdx + 1].name === '...');

            if (isEllipsis) {
                // Find pattern variables used in the repeating item
                const varsInItem = getPatternVars(item, bindings);

                if (varsInItem.length === 0) {
                    throw new Error("Ellipsis template must contain at least one pattern variable");
                }

                // Check lengths
                const lengths = varsInItem.map(v => {
                    const val = bindings.get(v);
                    if (!Array.isArray(val)) {
                        throw new Error(`Variable '${v}' used in ellipsis but not bound to a list`);
                    }
                    return val.length;
                });

                const len = lengths[0];
                if (!lengths.every(l => l === len)) {
                    throw new Error("Ellipsis expansion: variable lengths do not match");
                }

                // Expand N times
                for (let i = 0; i < len; i++) {
                    // Create a view of bindings for the i-th iteration
                    // We only need to override the vars involved in this ellipsis
                    const subBindings = new Map(bindings);
                    for (const v of varsInItem) {
                        subBindings.set(v, bindings.get(v)[i]);
                    }
                    result.push(transcribe(item, subBindings));
                }

                tIdx += 2; // Skip item and ...
            } else {
                result.push(transcribe(item, bindings));
                tIdx++;
            }
        }
        return result;
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
        if (node instanceof Variable) {
            if (bindings.has(node.name)) {
                vars.add(node.name);
            }
        } else if (Array.isArray(node)) {
            node.forEach(traverse);
        }
    }
    traverse(template);
    return Array.from(vars);
}
