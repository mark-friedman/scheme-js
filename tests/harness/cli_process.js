/**
 * Running the CLI, `repl.js`, in a child process, for the tests of what a
 * program it runs reads and writes (Node.js only).
 */

import path from 'path';
import { spawn } from 'child_process';
import { fileURLToPath } from 'url';

const ROOT = fileURLToPath(new URL('../../', import.meta.url));
const REPL = path.join(ROOT, 'repl.js');

/**
 * How long a run may take before it is taken to be waiting forever.
 * @type {number}
 */
const TIMEOUT_MS = 30000;

/**
 * Runs `repl.js` in a child process.
 * @param {Array<string>} args - Its arguments.
 * @param {Object} [options]
 * @param {string} [options.input=''] - Written to its standard input, which
 *   is then closed.
 * @param {number} [options.stdin] - A descriptor to give it as standard input
 *   instead of a pipe.
 * @param {number} [options.output] - A descriptor to give it as both standard
 *   output and standard error instead of two pipes, so that what it writes to
 *   each lands in one place, in the order it was written.
 * @param {string} [options.cwd] - The directory it runs in; the repository's
 *   root by default.
 * @param {Array<string>} [options.nodeOptions] - Options to Node itself,
 *   given before `repl.js`: `--inspect`, say.
 * @param {function(Object, string): void} [options.onOutput] - Called with the
 *   child and everything it has written to standard output so far, once when
 *   it starts and again each time it writes; when given, `input` is not
 *   written, and it is for this to write to the child's standard input and
 *   close it.
 * @returns {Promise<{status: number|null, stdout: string, stderr: string}>}
 *   How it exited and what it wrote.
 */
export function runCli(args, { input = '', stdin, output, cwd = ROOT, onOutput, nodeOptions = [] } = {}) {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [...nodeOptions, REPL, ...args], {
      cwd,
      stdio: [stdin ?? 'pipe', output ?? 'pipe', output ?? 'pipe']
    });
    let stdout = '';
    let stderr = '';
    const timer = setTimeout(() => {
      stderr += `\n(killed after ${TIMEOUT_MS} ms)`;
      child.kill();
    }, TIMEOUT_MS);
    if (child.stdout) {
      child.stdout.setEncoding('utf8');
      child.stdout.on('data', (data) => {
        stdout += data;
        if (onOutput) onOutput(child, stdout);
      });
    }
    if (child.stderr) {
      child.stderr.setEncoding('utf8');
      child.stderr.on('data', (data) => { stderr += data; });
    }
    child.on('close', (status) => {
      clearTimeout(timer);
      resolve({ status, stdout, stderr });
    });
    if (onOutput) onOutput(child, stdout);
    else if (stdin === undefined) child.stdin.end(input);
  });
}

/**
 * Writes to a child's standard input in steps, each once the child has
 * written a given text, and closes it after the last. A step waiting for what
 * the child answers to the step before holds its input open while the child
 * reads, which is how a person typing at it, or a process at the other end of
 * a pipe, would be.
 * @param {Array<{after: string, write: string}>} steps - What to write, and
 *   what the child must have written first.
 * @returns {function(Object, string): void} An `onOutput` for `runCli`.
 */
export function inSteps(steps) {
  let next = 0;
  return (child, stdout) => {
    while (next < steps.length && stdout.includes(steps[next].after)) {
      const { write } = steps[next++];
      if (next === steps.length) child.stdin.end(write);
      else child.stdin.write(write);
    }
  };
}

/**
 * What a run wrote, or its error output as well if it failed, so that a
 * failure's message says why.
 * @param {{status: number|null, stdout: string, stderr: string}} run - The run.
 * @returns {string} Its output.
 */
export function outputOf(run) {
  return run.status === 0 ? run.stdout : `${run.stdout}[exit ${run.status}] ${run.stderr}`;
}
