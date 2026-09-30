/**
 * Plain JavaScript versions of a few of the canonical programs, for
 * `compare_r7rs.js`.
 *
 * The founding question was how far this implementation is from plain
 * JavaScript -- 650x on `fib(30)`, interpreted. These answer it for the
 * compiled tier, on the programs where a plain version means something: each is
 * what a JavaScript programmer would write for the same work, with JavaScript
 * numbers throughout, so the gap includes everything Scheme's semantics cost --
 * exact integers, the full numeric tower, safe primitives -- and is not a
 * measure of the compiler alone.
 *
 * Run as `node plain_js_kernels.js <program>`, with the program's input on
 * standard input -- the repetition count, then the program's parameters and its
 * expected result, as `r7rs_harness.js` builds them -- and prints the same
 * result line the Scheme programs print.
 */

import fs from 'fs';

/**
 * The kernels, by program name. Each takes the parameters that follow the
 * count, and returns what the Scheme program's result is compared with.
 * @type {Object<string, Function>}
 */
const KERNELS = {
  fib: (n) => {
    const fib = (k) => (k < 2 ? k : fib(k - 1) + fib(k - 2));
    return fib(n);
  },
  tak: (x, y, z) => {
    const tak = (a, b, c) => (b >= a ? c : tak(tak(a - 1, b, c), tak(b - 1, c, a), tak(c - 1, a, b)));
    return tak(x, y, z);
  },
  ack: (m, n) => {
    const ack = (a, b) => (a === 0 ? b + 1 : b === 0 ? ack(a - 1, 1) : ack(a - 1, ack(a, b - 1)));
    return ack(m, n);
  },
  fibfp: (n) => {
    const fib = (k) => (k < 2 ? k : fib(k - 1) + fib(k - 2));
    return fib(n);
  },
  sum: (n) => {
    let sum = 0;
    for (let i = n; i >= 0; i--) sum += i;
    return sum;
  },
  sumfp: (n) => {
    let sum = 0;
    for (let i = n; i >= 0; i -= 1) sum += i;
    return sum;
  },
  nqueens: (n) => {
    const placed = [];
    const ok = (row) => {
      for (let d = 0; d < placed.length; d++) {
        const q = placed[placed.length - 1 - d];
        if (q === row + d + 1 || q === row - d - 1) return false;
      }
      return true;
    };
    const tryRows = (remaining) => {
      if (remaining.length === 0) return 1;
      let count = 0;
      for (let i = 0; i < remaining.length; i++) {
        const row = remaining[i];
        if (ok(row)) {
          placed.push(row);
          count += tryRows([...remaining.slice(0, i), ...remaining.slice(i + 1)]);
          placed.pop();
        }
      }
      return count;
    };
    return tryRows(Array.from({ length: n }, (_, i) => i + 1));
  }
};

/**
 * The programs with a plain version.
 * @type {Array<string>}
 */
export const PLAIN_JS_PROGRAMS = Object.keys(KERNELS);

/**
 * Runs one kernel as the Scheme program would be run: the count first, then
 * the parameters, then the expected result.
 * @returns {void}
 */
function main() {
  const name = process.argv[2];
  const kernel = KERNELS[name];
  if (!kernel) throw new Error(`no plain JavaScript version of ${name}`);
  const fields = fs.readFileSync(0, 'utf8').split(/\s+/).filter(Boolean).map(Number);
  const [count, ...rest] = fields;
  const expected = rest[rest.length - 1];
  const params = rest.slice(0, rest.length - 1);
  const start = performance.now();
  let result;
  for (let i = 0; i < count; i++) result = kernel(...params);
  const seconds = (performance.now() - start) / 1000;
  const label = `${name}:${params.join(':')}:${count}`;
  console.log(`+!CSVLINE!+plain-javascript,${label},${result === expected ? seconds : 'INCORRECT'}`);
}

if (process.argv[1] && process.argv[1].endsWith('plain_js_kernels.js')) main();
