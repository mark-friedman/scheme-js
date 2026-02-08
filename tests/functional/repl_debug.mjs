/**
 * @fileoverview Functional verification of REPL debugging.
 * Spawns the REPL and simulates user interaction.
 */

import { spawn } from 'child_process';

async function test() {
    console.log('Starting REPL Debugging functional test...');

    const repl = spawn('node', ['repl.js'], {
        stdio: ['pipe', 'pipe', 'inherit']
    });

    repl.stdout.setEncoding('utf8');

    const write = (cmd) => {
        console.log(`> ${cmd}`);
        repl.stdin.write(cmd + '\n');
    };

    let output = '';
    repl.stdout.on('data', (data) => {
        output += data;
        // console.log(data); // for debugging
    });

    const waitFor = (pattern, timeout = 5000) => {
        return new Promise((resolve, reject) => {
            const start = Date.now();
            const timer = setInterval(() => {
                if (output.includes(pattern)) {
                    clearInterval(timer);
                    resolve();
                }
                if (Date.now() - start > timeout) {
                    clearInterval(timer);
                    reject(new Error(`Timeout waiting for "${pattern}"\nOutput so far:\n${output}`));
                }
            }, 100);
        });
    };

    try {
        await waitFor('> ');
        console.log('REPL started');

        // 1. Test basic evaluation
        write('(+ 1 2)');
        await waitFor('3');
        console.log('Basic eval works');

        // 2. Test :debug command
        write(':debug');
        await waitFor('Debugging is OFF');
        console.log(':debug works');

        // 3. Test breakpoint and pause
        // Define a function
        write(':debug on');
        write('(define (test-break x) (+ x 1))');
        await waitFor('> ');

        // Set breakpoint (we don't know the exact "file" in REPL, but we can try)
        // Actually, REPL inputs are usually treated as "repl"
        // Let's use (pause) which is easier to trigger
        write('(test-break (pause))');

        await waitFor('Paused: manual pause');
        console.log('Debugger paused on (pause)!');

        // 4. Test debug commands while paused
        write(':bt');
        await waitFor('test-break');
        console.log(':bt works while paused');

        write(':locals');
        // We are inside the pause call, so locals might be tricky depending on where it was called
        // but it should at least show something
        await waitFor('Local variables');
        console.log(':locals works while paused');

        // 5. Test :eval while paused
        write('x'); // This is treated as :eval x in the debug loop
        await waitFor('result:');
        console.log('Eval in scope works while paused');

        // 6. Test :continue
        write(':c');
        await waitFor('> ');
        console.log('Resumed correctly');

        console.log('\nTEST PASSED!');
        repl.kill();
        process.exit(0);
    } catch (e) {
        console.error('\nTEST FAILED!');
        console.error(e.message);
        repl.kill();
        process.exit(1);
    }
}

test();
