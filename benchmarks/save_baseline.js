/**
 * Save current benchmark results as the baseline.
 * 
 * Usage:
 *   npm run benchmark:save
 *   node benchmarks/save_baseline.js
 */

import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function runBenchmarks() {
    return new Promise((resolve, reject) => {
        const benchmarkProcess = spawn('node', ['benchmarks/run_benchmarks.js'], {
            cwd: path.join(__dirname, '..'),
            stdio: ['inherit', 'pipe', 'inherit']
        });

        let output = '';
        benchmarkProcess.stdout.on('data', (data) => {
            const text = data.toString();
            process.stdout.write(text);
            output += text;
        });

        benchmarkProcess.on('close', (code) => {
            if (code !== 0) {
                reject(new Error(`Benchmark process exited with code ${code}`));
                return;
            }

            // Extract JSON from output
            const jsonMatch = output.match(/--- JSON Results ---\n([\s\S]*?)$/);
            if (!jsonMatch) {
                reject(new Error('Could not find JSON results in benchmark output'));
                return;
            }

            try {
                const results = JSON.parse(jsonMatch[1].trim());
                resolve(results);
            } catch (e) {
                reject(new Error(`Failed to parse JSON results: ${e.message}`));
            }
        });
    });
}

async function main() {
    console.log('Running benchmarks to create baseline...\n');

    const results = await runBenchmarks();

    const baseline = {
        timestamp: new Date().toISOString(),
        nodeVersion: process.version,
        results
    };

    const baselinePath = path.join(__dirname, 'baseline.json');
    fs.writeFileSync(baselinePath, JSON.stringify(baseline, null, 2));

    console.log(`\nBaseline saved to ${baselinePath}`);
    console.log(`Timestamp: ${baseline.timestamp}`);
    console.log(`Node version: ${baseline.nodeVersion}`);
}

main().catch(err => {
    console.error('Failed to save baseline:', err);
    process.exit(1);
});
