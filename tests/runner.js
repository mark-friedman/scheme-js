import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function main() {
    const args = process.argv.slice(2);
    const targetLayer = args[0] || '1'; // Default to Layer 1

    console.log(`\n=== Running Tests for Layer ${targetLayer} ===\n`);

    try {
        // 1. Dynamically import the Factory for the requested layer
        // We assume the directory naming convention: src/layer-N-*
        // Since we can't easily glob in pure node without deps, we'll map layers to paths for now
        // or just assume standard naming if we enforce it.
        // Let's use a map for robustness.
        const layerMap = {
            '1': '../src/layer-1-kernel/index.js',
            '2': '../src/layer-2-syntax/index.js',
            '3': '../src/layer-3-data/index.js',
            '4': '../src/layer-4-stdlib/index.js'
        };

        const factoryPath = layerMap[targetLayer];
        if (!factoryPath) {
            throw new Error(`Unknown layer: ${targetLayer}`);
        }

        console.log(`Loading Factory: ${factoryPath}`);
        const factoryModule = await import(factoryPath);
        const factory = factoryModule[`createLayer${targetLayer}`];

        if (!factory) {
            throw new Error(`Factory function createLayer${targetLayer} not found in ${factoryPath}`);
        }

        // 2. Instantiate the System Under Test
        const { interpreter, env } = factory();
        console.log(`Layer ${targetLayer} Interpreter Instantiated.`);

        // 3. Run the tests for this layer
        // We look for tests/layer-N/tests.js
        const testSuitePath = `./layer-${targetLayer}/tests.js`;
        console.log(`Loading Test Suite: ${testSuitePath}`);

        const testSuite = await import(testSuitePath);

        if (testSuite.run) {
            await testSuite.run(interpreter, env);
        } else {
            console.warn(`Test suite ${testSuitePath} does not export a 'run' function.`);
        }

        console.log(`\n=== Layer ${targetLayer} Tests Completed ===\n`);

    } catch (err) {
        console.error(`\n!!! Layer ${targetLayer} Test Failed !!!\n`);
        console.error(err);
        process.exit(1);
    }
}

main();
