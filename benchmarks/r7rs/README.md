# Canonical R7RS benchmarks

Vendored from [`ecraven/r7rs-benchmarks`](https://github.com/ecraven/r7rs-benchmarks),
upstream commit recorded in [`UPSTREAM_COMMIT`](UPSTREAM_COMMIT). Upstream states the
programs were *"Taken with kind permission from the Larceny project, based on the Gabriel
and Gambit benchmarks."* Individual files carry their own copyright notices where their
authors added them; those are left intact. Upstream ships no repository-level licence
file, which is worth knowing before these are redistributed further.

## Why a second benchmark suite

The eight programs in `benchmarks/programs/` were written in Stage 0 of the compiler
effort, against this implementation, and every optimization since was chosen by measuring
against them. Suite and optimizations ended up fitted to each other: 98% of the
microbenchmarks' procedure calls land on a primitive the compiler inlines, against 34% in
real code, and the ~12x the compiler tier reports there is 1.39x on the project's own
Scheme. That is recorded as R20 in [`docs/compiler_strategy.md`](../../docs/compiler_strategy.md).

These programs are the correction. Nobody on this project chose them, they predate it by
decades, they are what the Scheme implementation community actually quotes, and published
results exist for more than twenty implementations — so a Gambit or Racket number that
disagrees with the published one is evidence about *our harness* before it is evidence
about anything else.

They also cover workloads the eight do not touch at all: flonums, bignums, complex
numbers, bytevectors, strings, records, `dynamic-wind`, parsing, and symbolic processing
of a size no microbenchmark reaches.

## How a run is assembled

Upstream concatenates

    <implementation prelude>  <program>.scm  common.scm  common-postlude.scm

and pipes `inputs/<program>.input` to standard input. The program reads a repetition
count, its own parameters and the expected result, and hands them to
`run-r7rs-benchmark`, which runs the thunk that many times, **checks the result**, and
prints the elapsed time measured with R7RS `current-jiffy`.

Gambit and Racket are run exactly that way, with their own vendored preludes. This
implementation differs in two respects, both in `benchmarks/lib/r7rs_harness.js`:

- The `(import ...)` form is removed, and the interpreter is bootstrapped the way the
  rest of `benchmarks/` bootstraps one, so benchmark startup does not depend on the
  library loader.
- `read` called with no argument draws from the input text through a string port rather
  than from standard input, which has no meaning in a browser. A call *with* a port still
  reads from that port — `dynamic`, `read0`, `read1` and `sum1` open their own data
  files, and an earlier shim that ignored the argument made all four return wrong answers
  rather than fail.

The harness prelude is evaluated but never handed to the compiler tier. It is scaffolding
this project wrote; compiling it would mean reporting a measurement of the test rig as if
it were the workload.

## Sizes

Canonical sizes are out of reach: upstream's `fib` input asks for five repetitions of
`fib(40)`, which takes Gambit's *interpreter* 143 seconds. Programs that need a smaller
size carry replacement parameters in [`manifest.js`](manifest.js), written out in full so
that reading the manifest tells you exactly what ran. The canonical `.input` files stay in
the tree verbatim and are used unchanged wherever the manifest supplies no override, so
those results remain directly comparable to the published tables.

**Every replacement parameter set had its expected result derived from Gambit**, never
from our own output. A benchmark whose expected value came from the implementation under
test cannot detect that the implementation is wrong — and on this suite, three programs
turned out to be wrong.

Repetition counts are calibrated per implementation and results reported per iteration.
Racket's clock ticks a thousand times a second, so a count that gives this interpreter a
second of work gives Racket one or two ticks, and two ticks is not a measurement.

## What is not here

- `compiler.scm` (459 KB) — blocked on `string-set!` anyway, and large enough that
  vendoring it for a program we cannot run is not worth it.
- `cat`, `tail`, `wc`, `sum1` — need `inputs/bib` (4.5 MB) and `inputs/sum1.data`
  (1.1 MB). Host file I/O is also not a meaningful axis in a browser; the text axis
  deserves its own treatment rather than these.
- `mperm` — a GC benchmark that allocates over 100 MB by design.
- The remaining upstream programs and the other twenty-odd implementation preludes.

## Programs this implementation cannot run

Kept in the manifest rather than dropped: each is the evidence for a conformance gap, and
each becomes available the moment its gap closes. The suite found all four on its first
run.

| Programs | Gap |
|---|---|
| `gcbench`, `matrix`, `slatex` | Identifiers containing a dot are rejected by **extended dot notation**, a deliberate and tested interop feature. `(define x.y 1)` fails; R7RS §7.1.1 permits the dot. |
| `parsing`, `read0` | `read-char` and `peek-char` return JavaScript strings rather than Scheme characters, so `(char? (read-char p))` is `#f`. |
| `equal` | `equal?` does not terminate on circular structure, which R7RS §6.1 requires. Gambit runs this program in 0.08 s; we hang at every size. |
| `compiler` (not vendored) | `string-set!` throws unconditionally — the known interop-for-compliance trade. |

## Running

```
npm run benchmark:r7rs                  # this implementation, both tiers, by workload class
npm run benchmark:r7rs-implementations  # the same programs under Gambit and Racket
```

Both accept `--profile full` to include the `slow` entries — programs with no parameter
that can be reduced without changing what they measure — and `--target SECONDS` to change
the calibration target.
