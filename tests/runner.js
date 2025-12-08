import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function main() {
    console.log(`\n=== Running Runtime Tests ===\n`);

    try {
        // Load the runtime factory
        const factoryPath = '../src/runtime/index.js';
        console.log(`Loading Factory: ${factoryPath}`);
        const factoryModule = await import(factoryPath);
        const factory = factoryModule.createLayer1;

        if (!factory) {
            throw new Error(`Factory function createLayer1 not found in ${factoryPath}`);
        }

        // Instantiate the interpreter
        const { interpreter, env } = factory();
        console.log(`Runtime Interpreter Instantiated.`);

        // Run the tests
        const testSuitePath = `./runtime/tests.js`;
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

