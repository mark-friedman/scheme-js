# Numeric Tower Implementation Discussion

**Date:** January 9, 2026  
**Topic:** Feasibility of Hybrid Fixnum/Bignum Approach for Scheme-JS Numeric Tower

---

## Summary

This session explored whether the Scheme-JS interpreter should change its numeric tower implementation from using JavaScript BigInt as the base representation for exact integers to a hybrid approach that uses JavaScript Numbers for small integers and promotes to BigInt when values exceed the safe integer range.

### Key Findings

1. **Current Approach (BigInt base):**
   - All integer literals parsed as `BigInt` (exact)
   - Clean exactness semantics: `BigInt` = exact, `Number` = inexact
   - Requires conversion at JS boundaries (`BigInt` → `Number`)
   - Works correctly for large integers (factorial, cryptography, etc.)

2. **Proposed Hybrid Approach:**
   - Small integers stored as `Number` (within `±2^53`)
   - Promote to `BigInt` when results exceed safe range
   - **Pro:** Eliminates most JS boundary conversions
   - **Con:** Cannot distinguish exact vs inexact for small integers

3. **Core Trade-off:**
   - Hybrid approach cannot track exactness without wrapper classes
   - JavaScript's `5 === 5.0` makes it impossible to distinguish exact 5 from inexact 5.0
   - Wrapper classes add allocation overhead, negating performance benefits

4. **Recommendation:** Keep BigInt internally, optimize boundary conversions

---

## Full Conversation

### User's Initial Question

> I'm wondering if it would be feasible to change the numeric tower implementation to start by using JavaScript numbers (rather than JavaScript BigInts) as the base and promoting to BigInts when the results of numeric procedures return (or would return, if that can be known beforehand) numbers outside of the "safe" (i.e. exact) range of JS numbers? This seems somewhat similar to the approach of many Scheme implementations, which promote fixnums to bignums. What would be the pros and cons of such an approach, with regard to both the current approach used in this branch and the old approach of only using JS numbers?

### Analysis: Pros of Hybrid Approach

1. **Performance for Common Cases** - JavaScript engines are heavily optimized for `Number`. Operations like `+`, `*`, `<` on `Number` are typically 5-10× faster than on `BigInt`.

2. **Better JS Interoperability** - JS APIs expect `Number`, not `BigInt`. With small integers as `Number`, many conversions become unnecessary.

3. **Natural Match to Traditional Scheme Semantics** - Many Scheme implementations use exactly this approach (fixnum/bignum).

4. **Memory Efficiency** - `BigInt` objects have overhead compared to `Number`.

### Analysis: Cons of Hybrid Approach

1. **Implementation Complexity** - Every arithmetic operation must check for overflow and potentially promote.

2. **Exactness Ambiguity** - With hybrid, `Number` could be either exact (small integer) or inexact (float), breaking R7RS semantics.

3. **Unpredictable Promotion Boundaries** - Code that works with small numbers might break with large numbers.

4. **Edge Cases in Comparison** - Mixed-type comparisons become complex.

5. **Regression Risk** - Current system works well (982/1006 Chibi tests passing).

### User's Follow-up: JS Boundary Friction

> My thinking for this is that it would remove the need for the deep conversions at the JS boundaries, but maybe the overall complexity of the approach and performance implications in Scheme computations would make it infeasible. What do you think? Would combining this approach with explicit number type classes help?

### Analysis: Combining Hybrid + Type Classes

A design with wrapper classes (Fixnum/Bignum/Flonum) was explored:

```javascript
class Fixnum extends SchemeNumber {
    constructor(value) {
        super(value, true);  // Always exact
    }
}
```

**What this solves:**
- `exact?` just checks `obj._exact`
- Overflow detection before precision loss
- JS interop: `Fixnum._value` is already a JS `Number`

**Performance trade-off:**
- Object allocation for every arithmetic result
- Mitigation strategies: object pooling, inline caching

### User's Correction on Precision Loss

> You gave some issues with the new `add` function. One was that you said "// Promote: but now we've already lost precision!" Why have we lost precision? We haven't touched `a` or `b`.

### Clarification

The user was correct: if both operands are safe integers, they can always be recovered as BigInt exactly. The challenge is *detecting* when promotion is needed.

Solutions exist:
```javascript
// Recovery check
if (Number.isSafeInteger(result) && (result - a) === b) {
    return result;  // No rounding
}
return BigInt(a) + BigInt(b);
```

### Exactness Ambiguity Discussion

> I think that the exactness ambiguity might be the larger issue. I'm not sure how we could get around that. Would it work to represent it as the smallest non-safe floating point number to 5?

**Why this doesn't work:**
- JavaScript has exactly one 64-bit pattern for the value 5
- `5 === 5.0` is always true
- Changing the value (like `5.0000000001`) breaks arithmetic

**Only options for distinguishing exactness:**
1. **Type-based** (current): `BigInt` = exact, `Number` = inexact
2. **Wrapper class**: `new Exact(5)` vs `new Inexact(5.0)`
3. **Side table**: Complex, breaks identity
4. **Tagged union**: Same as wrapper class

### User's Decision Point

> Ok, explore option 3 in more depth.

### Option 3 Exploration: Keep BigInt, Optimize Boundaries

Six optimization strategies were analyzed:

1. **Lazy Conversion with Caching** - Convert only when needed, cache results
2. **Avoid Deep Conversion When Unnecessary** - Smart shallow convert for common cases
3. **"JS-Native" Mode for Hot Paths** - Opt-in mode using Numbers directly
4. **Profile-Guided Representation** - Track which closures cross JS boundaries frequently
5. **Reduce Conversion Locations** - Add `external` flag to skip internal conversions
6. **Inline Small Integer Optimization** - Precompute `MIN_SAFE_BIG`/`MAX_SAFE_BIG`

### Final Decision: Benchmark First

> Before I decide I'd like to run some tests to see what the performance differences are between the current approach in this branch and the other approach (currently in another branch), that uses JS Numbers, but with incorrect exactness. Can you suggest some tests I could run in both branches that I could then compare to see what the real performance profile is for both?

A comprehensive benchmark suite was designed:

**Arithmetic Benchmarks:**
- `sum-to-1M` - Small integer add/compare loop
- `factorial-100` - Large integer multiplication (correctness test!)
- `fib-30` - Recursive call + small arithmetic
- `division-chain-1K` - Rational creation
- `collatz-10K` - Mixed operations

**JS Interop Benchmarks:**
- `js-calls-10K` - Raw JS interop overhead
- `js-roundtrip-10K` - Passing numbers across boundary
- `array-convert-10K` - Deep conversion cost
- `nested-convert` - Nested list conversion

**Non-Numeric Baselines:**
- `list-reverse-100K` - List processing
- `map-filter-10K` - Higher-order functions
- `append-loop-1K` - Append-heavy workload
- `symbol-lookup-100K` - Symbol comparison

---

## Created Files

The following benchmark files were created:

1. `benchmarks/benchmark_arithmetic.scm` - Pure Scheme arithmetic tests
2. `benchmarks/benchmark_interop.scm` - JS interop tests
3. `benchmarks/benchmark_mixed.scm` - Non-numeric baseline tests
4. `benchmarks/run_benchmarks.js` - JavaScript test harness

---

## How to Run Benchmarks

```bash
# Branch 1: BigInt (current)
git checkout main  # or bigint-branch
node benchmarks/run_benchmarks.js > results_bigint.txt

# Branch 2: Number (old approach)
git checkout number-branch
node benchmarks/run_benchmarks.js > results_number.txt

# Compare
diff results_bigint.txt results_number.txt
```

---

## Key Questions the Benchmarks Answer

1. **How much does BigInt cost for typical small-integer code?** (sum-to, fib)
2. **How much does JS boundary crossing cost?** (js-calls, roundtrip)
3. **Is the overhead constant or proportional?** (compare 1K vs 10K iterations)
4. **What's the correctness cost of Number?** (factorial-100 gives wrong answer in Number branch!)

## Benchmark Results (BigInt branch):

| Test | Time (ms) | Category |
| :---- | :---- | :---- |
| sum-to-1M | 2672 | Arithmetic |
| factorial-100x1K | 297 | Arithmetic |
| fib-30 | 6136 | Arithmetic |
| division-chain-1K | 4 | Arithmetic |
| collatz-10K | 4831 | Arithmetic |
| list-reverse-100K | 1299 | Non-Numeric |
| map-filter-10K | 97 | Non-Numeric |
| append-loop-1K | 10 | Non-Numeric |
| symbol-lookup-100K | 1110 | Non-Numeric |
| js-calls-10K | 83 | JS Interop |
| js-roundtrip-10K | 46 | JS Interop |
| array-convert-10K | 77 | JS Interop |
| nested-convert | 40 | JS Interop |

Correctness: factorial(100) \= ✓ CORRECT (158 digits, starts with 93326215443944152681\)

---

Files created:

* benchmarks/benchmark\_arithmetic.scm \- sum, factorial, fib, division, collatz  
* benchmarks/benchmark\_interop.scm \- JS calls, roundtrip, array/list conversion  
* benchmarks/benchmark\_mixed.scm \- list reverse, map/fold, append, symbol lookup  
* benchmarks/run\_benchmarks.js \- Test harness  
* numeric\_tower\_discussion.md \- Session transcript

To run on the Number branch:

```bash  
git stash  \# or commit your changes  
git checkout \<number-branch\>  
node benchmarks/run\_benchmarks.js  
```
The key comparison points will be:

* factorial-100x1K: Will show incorrect results on Number branch (precision loss)  
* sum-to-1M, fib-30, collatz-10K: May be faster on Number branch (native Number ops)  
* JS Interop tests: May be faster on Number branch (no BigInt conversion)
