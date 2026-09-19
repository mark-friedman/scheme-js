/**
 * @fileoverview Inline expansions for primitives, used by code generation.
 *
 * Profiling the first compiler tier put 25% of its runtime in primitive calls
 * and only 17% in the generated code itself. A call such as `(+ a b)` was going
 * through a variadic primitive that allocates a rest array, type-checks each
 * argument and dispatches across the numeric tower -- all to add two integers.
 *
 * Each entry here gives a fast path for the overwhelmingly common operand shape
 * and falls back to the real primitive for everything else, so the numeric
 * tower's semantics are preserved rather than approximated: a rational, a
 * flonum, a complex or a wrong type all take the fallback and behave exactly as
 * they do in the interpreter.
 *
 * Every expansion is also **guarded on the binding**. Scheme allows `+` to be
 * redefined, so generated code compares the global against the primitive that
 * was bound when it was compiled and calls through the global if they differ.
 * Inlining without that guard would silently ignore a redefinition.
 */

/**
 * @typedef {Object} Inlinable
 * @property {number} arity - Exact argument count this expansion handles.
 * @property {function(Array<string>): (string|null)} test - Given JavaScript
 *   expressions for the arguments, returns the condition under which the fast
 *   path is valid, or null if it is always valid.
 * @property {function(Array<string>): string} value - The fast-path expression.
 */

/** Tests that both operands are exact integers, the dominant case. */
const bothExact = ([a, b]) => `typeof ${a} === 'bigint' && typeof ${b} === 'bigint'`;

/** Fast path applies to every input the primitive accepts. */
const always = () => null;

/** @type {Object<string, Inlinable>} */
export const INLINABLE = {
  // --- arithmetic: exact-integer fast path, tower fallback -----------------
  // The fallback is what preserves the numeric tower: a rational, a flonum, a
  // complex or a wrong type all take it and behave exactly as interpreted.
  '+': { arity: 2, test: bothExact, value: ([a, b]) => `${a} + ${b}` },
  '-': { arity: 2, test: bothExact, value: ([a, b]) => `${a} - ${b}` },
  '*': { arity: 2, test: bothExact, value: ([a, b]) => `${a} * ${b}` },
  '<': { arity: 2, test: bothExact, value: ([a, b]) => `${a} < ${b}` },
  '>': { arity: 2, test: bothExact, value: ([a, b]) => `${a} > ${b}` },
  '<=': { arity: 2, test: bothExact, value: ([a, b]) => `${a} <= ${b}` },
  '>=': { arity: 2, test: bothExact, value: ([a, b]) => `${a} >= ${b}` },
  '=': { arity: 2, test: bothExact, value: ([a, b]) => `${a} === ${b}` },

  // --- pairs: the representation is a plain class, so these are direct ----
  'car': { arity: 1, test: ([a]) => `${a} instanceof R.Cons`, value: ([a]) => `${a}.car` },
  'cdr': { arity: 1, test: ([a]) => `${a} instanceof R.Cons`, value: ([a]) => `${a}.cdr` },

  // --- total expansions ---------------------------------------------------
  // No type test: the fast path is exactly what the primitive computes for
  // every input, so only the binding guard applies.
  'cons': { arity: 2, test: always, value: ([a, b]) => `new R.Cons(${a}, ${b})` },
  'pair?': { arity: 1, test: always, value: ([a]) => `${a} instanceof R.Cons` },
  'null?': { arity: 1, test: always, value: ([a]) => `${a} === null` },
  'not': { arity: 1, test: always, value: ([a]) => `${a} === false` },
  'eq?': { arity: 2, test: always, value: ([a, b]) => `${a} === ${b}` }
};

/**
 * Looks up an inline expansion for a call.
 * @param {string} name - The primitive's name.
 * @param {number} argCount - Number of arguments at the call site.
 * @returns {Inlinable|null} The expansion, or null if there is none.
 */
export function inlinableFor(name, argCount) {
  const entry = INLINABLE[name];
  return entry && entry.arity === argCount ? entry : null;
}
