import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function main() {
    console.log(`\n=== Running Core Tests ===\n`);

    try {
        // Load the interpreter factory
        const factoryPath = '../src/core/interpreter/index.js';
        console.log(`Loading Factory: ${factoryPath}`);
        const factoryModule = await import(factoryPath);
        const factory = factoryModule.createInterpreter;

        if (!factory) {
            throw new Error(`Factory function createInterpreter not found in ${factoryPath}`);
        }

        // Instantiate the interpreter
        const { interpreter, env } = factory();
        console.log(`Interpreter Instantiated.`);

        // Run the tests
        const testSuitePath = `./core/interpreter/tests.js`;
        console.log(`Loading Test Suite: ${testSuitePath}`);

        const testSuite = await import(testSuitePath);

        if (testSuite.run) {
            await testSuite.run(interpreter, env);
        } else {
            console.warn(`Test suite ${testSuitePath} does not export a 'run' function.`);
        }

        console.log(`\n=== Runtime Tests Completed ===\n`);

    } catch (err) {
        console.error(`\n!!! Runtime Test Failed !!!\n`);
        console.error(err);
        process.exit(1);
    }
}

main();

