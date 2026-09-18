# Stage 2a — calling-convention bake-off

Throwaway prototypes, not production code. Their only job is to settle the calling-convention
decision in [../../docs/compiler_strategy.md](../../docs/compiler_strategy.md) with measurements
instead of priors, and then be deleted or rewritten properly in Stage 2b.

Two conventions are compiled from the same front end, so differences in the numbers are differences
between the conventions and not between two compilers:

- **A — explicit frame stack.** Continuation frames live in a JavaScript array and everything runs
  under a trampoline. The JavaScript stack stays one frame deep. This is the Gambit-JS model
  (Thivierge & Feeley, SFP 2012).
- **B — native JavaScript stack.** Non-tail calls are ordinary JavaScript calls, so one live Scheme
  frame is one live JavaScript frame; tail calls return through a per-call-site trampoline;
  `call/cc` reifies the stack by a cooperative unwind. This is Pettyjohn et al.'s generalized stack
  inspection with Marshall's distinguished-return-value variant.

Deliberately *not* optimized: no self-tail-call-to-loop conversion, no inlining, no unboxing. Both
backends get the same treatment so the comparison isolates the convention.

## Running

```bash
node experiments/stage2a/run.js              # correctness + timings for both backends
node experiments/stage2a/stack_shape.js      # what a JS stack trace shows under each
node experiments/stage2a/emit.js fib A       # dump generated code for inspection
```
