/**
 * @fileoverview One start of the runtime in this process, phase by phase, for
 * `benchmarks/run_startup.js`, which runs it in a fresh process each time:
 * importing the runtime's modules, making an interpreter, importing
 * `(scheme base)` and `(scheme write)` as a page does -- each restored from its
 * prebuilt table -- and starting the compiler. Prints each phase's
 * milliseconds as JSON.
 */

const t0 = performance.now();
const { createInterpreter } = await import('../../src/core/interpreter/index.js');
const { parse } = await import('../../src/core/interpreter/reader.js');
const { analyze } = await import('../../src/core/interpreter/expand.js');
const { setFileResolver, setLibraryLoadHook, setLibraryRestorer, programEnvironment, runProgramForm } =
  await import('../../src/core/interpreter/library_loader.js');
const { installLibraryTable, libraryRestorer } = await import('../../src/compiler/prebuilt.js');
const { default: prebuiltLibraries } = await import('../../src/packaging/compiled_libraries.js');
const { BUNDLED_SOURCES } = await import('../../src/packaging/bundled_libraries.js');
const { compilerStartFailure } = await import('../../src/compiler/lowering.js');
const t1 = performance.now();

const { interpreter, env } = createInterpreter();
const t2 = performance.now();

setFileResolver((name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] ?? BUNDLED_SOURCES[name[name.length - 1]]);
setLibraryRestorer(libraryRestorer(prebuiltLibraries));
setLibraryLoadHook((name, libraryEnv) => {
  if (libraryEnv) installLibraryTable(prebuiltLibraries, name, libraryEnv, (file) => BUNDLED_SOURCES[file]);
});
const program = programEnvironment(parse('(import (scheme base) (scheme write)) (+ 1 2)'), analyze, interpreter, env);
for (const form of program.forms) runProgramForm(form, analyze, interpreter, program.env, { jsAutoConvert: 'raw' });
const t3 = performance.now();

const failure = compilerStartFailure();
const t4 = performance.now();
if (failure !== null) throw new Error(`the compiler could not start: ${failure}`);

console.log(JSON.stringify({ modules: t1 - t0, interpreter: t2 - t1, libraries: t3 - t2, compiler: t4 - t3 }));
