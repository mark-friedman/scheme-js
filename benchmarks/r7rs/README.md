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
the test files, and the ~12x the compiler tier reports there is 1.39x on the project's own
Scheme. That is recorded as R20 in [`docs/compiler_findings.md`](../../docs/compiler_findings.md).

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

- `cat`, `tail`, `wc`, `sum1` — need `inputs/bib` (4.5 MB) and `inputs/sum1.data`
  (1.1 MB). Host file I/O is also not a meaningful axis in a browser; the text axis
  deserves its own treatment rather than these.
- `mperm` — a GC benchmark that allocates over 100 MB by design.
- The remaining upstream programs and the other twenty-odd implementation preludes.

## Programs this implementation cannot run

Kept in the manifest rather than dropped: each is the evidence for a conformance gap, and
each becomes available the moment its gap closes. The suite found four on its first run.
`parsing` has run since its gap closed; `gcbench`, `matrix` and `slatex` since the harness reads
every program with dot notation off -- they name procedures like `node.left`, which R7RS §7.1.1
allows; and `equal` since `equal?` terminates on circular structure, though slowly (`'slow'`:
about 444 s an iteration interpreted, 12 s compiled, where Gambit takes 0.08 s).

| Programs | Gap |
|---|---|
| `read0` | Does not finish within the correctness runner's 120 s in either tier: it reads every two-character string from `a` and U+0000 to `a` and U+10FFFF, twice each, and exercises reader syntax we do not accept. It and `parsing` were also blocked on `read-char` and `peek-char` returning strings rather than characters; `parsing` runs since that was fixed. |

## Running

```
npm run benchmark:r7rs                  # this implementation, both tiers, by workload class
npm run benchmark:r7rs-implementations  # both tiers against Gambit (gsi, C, JS), Racket and plain JS
```

Both accept `--profile full` to include the `slow` entries — programs with no parameter
that can be reduced without changing what they measure — and `--target SECONDS` to change
the calibration target.

The comparison measures both of our tiers again unless told not to. Measuring us is most of
its running time, and the references do not change when our code does, so it can take our
figures from a saved `benchmark:r7rs` run instead; `--tiers interpreter` or `--tiers compiled`
narrows it to one tier:

```
npm run benchmark:r7rs > ours.log
npm run benchmark:r7rs-implementations -- --ours ours.log
```

### Gambit's compiler

`gsc` builds each program twice, with upstream's `GambitC-prelude.scm` declarations, and
the harness runs each build at every calibrated count:

- **to C**, a native executable;
- **to JavaScript** (`-target js`), one self-contained file run by the same Node as
  scheme-js-4. Same engine, same target language: the reference closest to our compiler.
  It needs nothing beyond Gambit and Node. (Gambit 4.9.5 has no WebAssembly target —
  `gsc -target wasm` reports the module unavailable.)

The C build needs the C compiler Gambit was configured with, and a Homebrew Gambit names a
versioned GCC — `gsc -exe` fails with
`gcc-13: command not found` when a different one is installed. Any recent GCC works: put a
link under the configured name ahead of it on `PATH`, for the run only. Passing `-cc` to
`gsc` instead is not a substitute, since it also drops every C flag Gambit was configured
with. The harness already passes the define that lets code from a newer compiler load into
the older runtime; see `GSC_C_OPTIONS` in `../compare_r7rs.js`.

```
mkdir -p /tmp/gccshim && ln -sf "$(command -v gcc-16)" /tmp/gccshim/gcc-13
PATH=/tmp/gccshim:$PATH npm run benchmark:r7rs-implementations
```

On macOS, if the comparison reports Gambit compiled to C as having no C toolchain, or the
compiler reports that `xcodebuild` failed while looking for `as`, the selected Xcode lacks
its compiler or is broken; `DEVELOPER_DIR=/Library/Developer/CommandLineTools` on the same
command uses the Command Line Tools instead. If a trial build still fails, Gambit compiled
to C is reported as not available, with the reason, and the comparison runs without it.
