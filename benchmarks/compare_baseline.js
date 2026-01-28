/**
 * Compare current benchmark results against stored baseline.
 * 
 * Usage:
 *   npm run benchmark:compare
 *   node benchmarks/compare_baseline.js
 * 
 * Exit codes:
 *   0 - All tests within acceptable range
 *   1 - Severe regression detected (>50% slower)
 */

import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const WARN_THRESHOLD = 0.20;   // 20% slower = warning
const FAIL_THRESHOLD = 0.50;   // 50% slower = failure

async function runBenchmarks() {
    return new Promise((resolve, reject) => {
        const benchmarkProcess = spawn('node', ['benchmarks/run_benchmarks.js'], {
            cwd: path.join(__dirname, '..'),
            stdio: ['inherit', 'pipe', 'inherit']
        });

        let output = '';
        benchmarkProcess.stdout.on('data', (data) => {
            output += data.toString();
        });

        benchmarkProcess.on('close', (code) => {
            if (code !== 0) {
                reject(new Error(`Benchmark process exited with code ${code}`));
                return;
            }

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

function formatChange(baseline, current) {
    if (baseline === null || current === null) return 'N/A';
    const change = ((current - baseline) / baseline) * 100;
    const sign = change >= 0 ? '+' : '';
    return `${sign}${change.toFixed(1)}%`;
}

function getStatus(baseline, current) {
    if (baseline === null || current === null) return '⚪';
    const change = (current - baseline) / baseline;
    if (change > FAIL_THRESHOLD) return '🔴'; // severe regression
    if (change > WARN_THRESHOLD) return '🟡'; // warning
    if (change < -WARN_THRESHOLD) return '🟢'; // significant improvement
    return '⚪'; // within normal range
}

async function main() {
    const baselinePath = path.join(__dirname, 'baseline.json');

    if (!fs.existsSync(baselinePath)) {
        console.log('No baseline found. Run `npm run benchmark:save` first.');
        console.log('Running benchmarks without comparison...\n');
        await runBenchmarks();
        return;
    }

    const baselineData = JSON.parse(fs.readFileSync(baselinePath, 'utf8'));

    console.log('Running benchmarks for comparison...\n');
    const currentResults = await runBenchmarks();

    console.log('\n' + '='.repeat(70));
    console.log('BASELINE COMPARISON');
    console.log('='.repeat(70));
    console.log(`Baseline from: ${baselineData.timestamp}`);
    console.log(`Baseline Node: ${baselineData.nodeVersion}`);
    console.log(`Current Node:  ${process.version}`);
    console.log('');

    console.log('| Benchmark | Baseline | Current | Change | Status |');
    console.log('|-----------|----------|---------|--------|--------|');

    let hasWarning = false;
    let hasSevere = false;

    for (const [name, currentValue] of Object.entries(currentResults)) {
        const baselineValue = baselineData.results[name];
        const change = formatChange(baselineValue, currentValue);
        const status = getStatus(baselineValue, currentValue);

        const baseDisplay = baselineValue !== null ? baselineValue : 'N/A';
        const currDisplay = currentValue !== null ? currentValue : 'N/A';

        console.log(`| ${name.padEnd(20)} | ${String(baseDisplay).padEnd(8)} | ${String(currDisplay).padEnd(7)} | ${change.padEnd(6)} | ${status} |`);

        if (status === '🟡') hasWarning = true;
        if (status === '🔴') hasSevere = true;
    }

    console.log('');

    if (hasSevere) {
        console.log('❌ SEVERE REGRESSION DETECTED (>50% slower on some benchmarks)');
        process.exit(1);
    } else if (hasWarning) {
        console.log('⚠️  Warning: Some benchmarks are >20% slower than baseline');
    } else {
        console.log('✅ All benchmarks within acceptable range');
    }
}

main().catch(err => {
    console.error('Comparison failed:', err);
    process.exit(1);
});
