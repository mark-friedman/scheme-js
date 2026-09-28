# Why the compiler tier declines procedures in real R7RS code

Measured 2026-09-27, on the `compiler-investigation` branch, by `benchmarks/decline_reasons.js
--corpus` over the corpus recorded in `benchmarks/corpus/manifest.json`. The benchmark programs
and this repository's own Scheme were written to avoid the control forms, so they cannot say what
the tier's decline policy costs a real program; this corpus is other people's code.

To repeat it:

```bash
node benchmarks/corpus/fetch.js
node benchmarks/decline_reasons.js --corpus --files --reasons
node benchmarks/run_escapes.js
```

`fetch.js` downloads exactly what the manifest records -- each SRFI repository at a commit, each
Snow-Fort package at a version, checked against the SHA-256 the Snow-Fort index gives -- into
`benchmarks/corpus/downloads/`, which is not committed.

## The corpus

| | sources | what |
|---|---|---|
| SRFI reference implementations | 7 | SRFIs 41, 64, 113, 130, 135, 146, 158 |
| Snow-Fort packages | 17 | parsers (`(okmij ssax)`, `(chibi parse)`, `(macduffie json)`, `(edn)`, `(rapid read)`), backtracking (`(rebottled schelog)`), regular expressions (`(chibi irregex)`, `(rebottled pregexp)`, `(chibi regexp)`), formatting and templating (`(chibi show)`, `(arvyy mustache)`, `(slib pretty-print)`), functional data structures (`(pfds fingertree)`, `(pfds hash-array-mapped-trie)`, `(yasos)`), test frameworks (`(chibi test)`, `(nytpu contracts)`) |
| dependencies | 37 | what those import and this implementation does not provide: SRFIs 143 and 151 from their repositories, the rest from Snow-Fort |

Each library is loaded and its procedures put to the policy the build applies to a library
(`generateEnvironment`, the procedures it defines itself). SRFI 64's `testing.scm` and SRFI 41's
`r5rs.ss` are programs, and are measured as the repository's files are.

## Results

62 libraries and programs measured: 1,920 top-level definitions, 1,855 of them procedures.

| outcome | procedures | share |
|---|---|---|
| compiles | 1,449 | 78.1% |
| reaches a capture through another procedure | 305 | 16.4% |
| captures a continuation | 94 | 5.1% |
| `with-exception-handler` (or `guard`, which expands into it) | 2 | 0.1% |
| `dynamic-wind`, directly or through `parameterize` | 3 | 0.2% |
| `exit`, `eval` | 2 | 0.1% |

**Of the 406 declined for a control form, 399 -- 98% -- end at `call/cc`.** `guard`,
`with-exception-handler`, `parameterize` and `dynamic-wind` together decline five.

Counted in every corpus source file, including the libraries that could not be measured: 72 uses
of `call/cc`, 16 of `parameterize`, 8 of `guard`, 4 of `dynamic-wind`, 2 of `raise` and none of
`with-exception-handler` -- against 515 of `error`, which compiles. Most of the `guard` and
`parameterize` uses are in the Chibi libraries below that cannot yet be loaded, so those forms will
matter more once they can, but `call/cc` still has more uses than all of them together.

The declines concentrate where one capture is reached by a whole library: SRFI 146 (100 of 104
procedures), SRFI 146's hash maps (64 of 79), `(rapid mapping ordered)` (91 of 95), SRFI 113 (43 of
180), `(rapid list)` (33 of 103), the two red-black trees (19 and 18).

## What the captures are for

Read site by site, nearly all are **escapes**: the continuation is called once, before the capture
returns, to leave early -- `(call/cc (lambda (return) ... (return x) ...))`, often from inside a
callback given to `for-each` or a search. SRFI 146's tree matches patterns through a macro that
expands into one, so every procedure using it captures. The SRFI 1 code in `(rapid list)` says so in
a comment: it uses `call/cc` "to do local aborts", and hopes for a compiler that handles that
efficiently. Only three libraries re-enter a continuation: SRFI 158's and `(rapid generator)`'s
coroutine generators, and Schelog's backtracking -- 31 of the 399 declines.

The policy that declines them was justified on `btsearch`, a backtracking search, where compiling
the captures made it 2x slower. `run_escapes.js` times the escape shape instead. Microseconds per
search, with the frames beneath the capture compiled wherever the policy allows:

| shape | frames beneath | interpreted | default policy | captures compiled | captures vs default |
|---|---|---|---|---|---|
| return from a callback | 0 | 28.8 | 26.9 | 10.1 | 2.66x |
| | 10 | 38.2 | 26.4 | 11.2 | 2.37x |
| | 50 | 105.6 | 33.3 | 15.8 | 2.11x |
| abort from a recursion | 0 | 7.4 | 4.5 | 1.2 | 3.89x |
| | 10 | 17.6 | 5.7 | 2.3 | 2.53x |
| | 50 | 62.5 | 9.9 | 6.7 | 1.48x |

For escapes the default is the slower choice at every depth measured, by 1.5-3.9x. The advantage
narrows with depth, because each capture unwinds and reifies the compiled frames beneath it; deeper
stacks than fifty were not measured.

## What could not be measured, and why

21 libraries. None for a reason the tier has anything to do with.

| libraries | reason |
|---|---|
| `(srfi 14)`, and through it `(chibi string)`, `(chibi parse)`, `(chibi parse common)`, `(edn)`, `(chibi regexp)`, `(chibi regexp pcre)`, `(chibi char-set boundary)`, `(chibi show)`, `(chibi show base)`, `(chibi show pretty)` | `string-set!` while loading: strings are immutable here |
| `(srfi 135)`, `(srfi 135 kernel8)` | an identifier with a dot in it, `length&i0.length`, is read as JavaScript property access |
| `(rapid match)`, and through it `(rapid syntax)` | `(import (rename (scheme base) (... ellipsis)))`: import filters are not applied to macros or syntax keywords, so `ellipsis` is never bound |
| `(srfi 64)`, `(srfi 64 execution)`, `(srfi 64 test-runner-simple)`, `(srfi 64 source-info)` | need SRFIs 35 and 48, or `(rnrs syntax-case)`, which have no R7RS implementation to fetch |
| `(srfi 130)` | needs SRFI 13, likewise |

Four more failures, found while bringing the corpus up, were bugs in this implementation, and are
fixed: `(scheme inexact)` could not be imported; `rename` in an import set crashed on the syntax
R7RS gives it, and nested import sets applied their filters in a fixed order rather than inside
out; a line comment ending in CR LF or CR swallowed the rest of the file, so SRFI 41's `r5rs.ss`
read as empty; and `#u8(#x41)` was rejected, while `#u8(65.5)` was read as `#u8(65)`.

## Re-measured once strings could be changed (2026-09-28)

Task 49 made newly allocated strings mutable, which was what kept `(srfi 14)` and the ten Chibi
libraries built on it from loading, and `(chibi monad environment)` was added to the manifest as the
dependency `(chibi show)` then asked for. 2,235 top-level definitions are measured, from 13
libraries unmeasured rather than 21, and the conclusion stands: of 407 declines for a control form,
399 (98%) end at `call/cc`, and `with-exception-handler` or `guard` accounts for 3.

| outcome | definitions | share |
|---|---|---|
| compiles | 1,763 | 78.9% |
| reaches a capture through another procedure | 305 | 13.6% |
| captures a continuation | 94 | 4.2% |
| the definition is not a procedure | 65 | 2.9% |
| another control form | 8 | 0.4% |

Still unmeasured: SRFI 64's R7RS libraries and SRFI 130, for dependencies with no R7RS
implementation; SRFI 135, for dot notation; `(rapid match)` and the two libraries importing it,
for import filters that do not reach macros; and `(chibi show)` and its two sublibraries, which
now fail inside the expansion of `(chibi monad environment)`'s `fn` macro -- not investigated yet.
