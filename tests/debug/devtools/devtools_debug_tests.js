/**
 * @fileoverview Unit tests for DevToolsDebugIntegration.
 *
 * Tests the core probe-calling mechanism: maybeHit() deduplication,
 * probe invocation, environment proxy caching, and integration with
 * the interpreter trampoline.
 *
 * Part of Phase 2: Trampoline Integration & Stepping.
 */

import { assert } from '../../harness/helpers.js';
import { DevToolsDebugIntegration } from '../../../src/debug/devtools/devtools_debug.js';
import { SchemeSourceRegistry } from '../../../src/debug/devtools/source_registry.js';
import { Environment } from '../../../src/core/interpreter/environment.js';
import { createInterpreter } from '../../../src/core/interpreter/index.js';
import { parse } from '../../../src/core/interpreter/reader.js';
import { analyze } from '../../../src/core/interpreter/analyzer.js';
import { list } from '../../../src/core/interpreter/cons.js';
import { intern } from '../../../src/core/interpreter/symbol.js';

/**
 * Creates a DevToolsDebugIntegration with a source pre-registered.
 *
 * @param {string} url - Scheme source URL
 * @param {string} content - Scheme source code
 * @returns {{devtools: DevToolsDebugIntegration, registry: SchemeSourceRegistry}}
 */
function createTestDevTools(url, content) {
  const registry = new SchemeSourceRegistry();
  const exprs = parse(content, { filename: url });
  registry.register(url, content, 'external', exprs);
  const devtools = new DevToolsDebugIntegration(registry);
  devtools.enable();
  return { devtools, registry };
}

/**
 * Runs all DevToolsDebugIntegration tests.
 * @param {Object} logger - Test logger
 */
export async function runDevToolsDebugTests(logger) {

  // =========================================================================
  // Construction and Enable/Disable
  // =========================================================================
  logger.title('DevToolsDebugIntegration - Construction');

  // Test: constructor initializes state
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    assert(logger, 'starts disabled', devtools.enabled, false);
    assert(logger, 'lastHitKey is null', devtools.lastHitKey, null);
    assert(logger, 'hitCount is 0', devtools.hitCount, 0);
    assert(logger, 'sourceRegistry is set', devtools.sourceRegistry === registry, true);
  }

  // Test: enable/disable
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    devtools.enable();
    assert(logger, 'enable sets enabled to true', devtools.enabled, true);

    devtools.disable();
    assert(logger, 'disable sets enabled to false', devtools.enabled, false);
    assert(logger, 'disable clears lastHitKey', devtools.lastHitKey, null);
  }

  // =========================================================================
  // maybeHit - Basic Probe Calling
  // =========================================================================
  logger.title('DevToolsDebugIntegration - maybeHit Basic');

  // Test: maybeHit calls probe for registered source
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n(+ 3 4)\n'
    );

    let probeCalled = false;
    // Replace the probe with a spy
    const exprId = devtools.sourceRegistry.getExprId('scheme://app/test.scm', 1, 1);
    const origProbe = devtools.sourceRegistry.getProbe('scheme://app/test.scm', exprId);
    assert(logger, 'probe exists for line 1', typeof origProbe, 'function');

    devtools.enableTracking();
    const env = new Environment();
    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env);

    assert(logger, 'hitCount increments on probe call', devtools.hitCount, 1);
    assert(logger, 'hit history records the key', devtools.getHitHistory().length, 1);
    assert(logger, 'hit history has correct key',
      devtools.getHitHistory()[0], 'scheme://app/test.scm:1:1');
  }

  // Test: maybeHit skips same location + same env (deduplication)
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n(+ 3 4)\n'
    );

    devtools.enableTracking();
    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env);
    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env);
    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 5 }, env); // No expressions at col 5, ignored

    assert(logger, 'same location + same env only fires once', devtools.hitCount, 1);
    assert(logger, 'hit history has only 1 entry', devtools.getHitHistory().length, 1);
  }

  // Test: maybeHit fires on same line with different env (recursive call re-entry)
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n(+ 3 4)\n'
    );

    devtools.enableTracking();
    const env1 = new Environment();
    const env2 = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env1);
    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env2);

    assert(logger, 'same location + different env fires twice', devtools.hitCount, 2);
  }

  // Test: maybeHit fires on line change
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n(+ 3 4)\n'
    );

    devtools.enableTracking();
    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env);
    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 2, column: 1 }, env);

    assert(logger, 'fires twice for different lines', devtools.hitCount, 2);

    const history = devtools.getHitHistory();
    assert(logger, 'history has 2 entries', history.length, 2);
    assert(logger, 'first entry is line 1', history[0], 'scheme://app/test.scm:1:1');
    assert(logger, 'second entry is line 2', history[1], 'scheme://app/test.scm:2:1');
  }

  // Test: maybeHit skips unregistered source
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n'
    );

    devtools.enableTracking();
    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/unknown.scm', line: 1, column: 1 }, env);

    assert(logger, 'unregistered source does not fire probe', devtools.hitCount, 0);
  }

  // Test: maybeHit skips out-of-range line
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n'
    );

    devtools.enableTracking();
    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 99, column: 1 }, env);

    assert(logger, 'out-of-range line does not fire probe', devtools.hitCount, 0);
  }

  // =========================================================================
  // maybeHit - Environment Proxy Caching
  // =========================================================================
  logger.title('DevToolsDebugIntegration - Env Proxy Caching');

  // Test: same env reuses proxy
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n(+ 3 4)\n'
    );

    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env);
    const proxy1 = devtools.currentEnvProxy;

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 2, column: 1 }, env);
    const proxy2 = devtools.currentEnvProxy;

    assert(logger, 'same env reuses proxy', proxy1 === proxy2, true);
  }

  // Test: different env creates new proxy
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n(+ 3 4)\n'
    );

    const env1 = new Environment();
    const env2 = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env1);
    const proxy1 = devtools.currentEnvProxy;

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 2, column: 1 }, env2);
    const proxy2 = devtools.currentEnvProxy;

    assert(logger, 'different env creates new proxy', proxy1 === proxy2, false);
  }

  // =========================================================================
  // resetHitTracking
  // =========================================================================
  logger.title('DevToolsDebugIntegration - Reset');

  // Test: resetHitTracking allows re-firing same line
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n'
    );

    devtools.enableTracking();
    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env);
    assert(logger, 'first hit fires', devtools.hitCount, 1);

    devtools.resetHitTracking();
    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env);
    assert(logger, 'after reset, same location fires again', devtools.hitCount, 2);
  }

  // =========================================================================
  // enableTracking / disableTracking
  // =========================================================================
  logger.title('DevToolsDebugIntegration - Tracking');

  // Test: disableTracking returns and clears history
  {
    const { devtools } = createTestDevTools(
      'scheme://app/test.scm',
      '(+ 1 2)\n(+ 3 4)\n'
    );

    devtools.enableTracking();
    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 1, column: 1 }, env);
    devtools.maybeHit({ filename: 'scheme://app/test.scm', line: 2, column: 1 }, env);

    const history = devtools.disableTracking();
    assert(logger, 'disableTracking returns history', history.length, 2);

    // After disable, getHitHistory returns empty
    assert(logger, 'after disable, history is empty', devtools.getHitHistory().length, 0);
  }

  // =========================================================================
  // Multi-file tracking
  // =========================================================================
  logger.title('DevToolsDebugIntegration - Multi-file');

  // Test: maybeHit tracks across multiple files
  {
    const registry = new SchemeSourceRegistry();
    const exprsA = parse('(define x 1)\n', { filename: 'scheme://app/a.scm' });
    registry.register('scheme://app/a.scm', '(define x 1)\n', 'external', exprsA);
    const exprsB = parse('(define y 2)\n', { filename: 'scheme://app/b.scm' });
    registry.register('scheme://app/b.scm', '(define y 2)\n', 'external', exprsB);
    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();
    devtools.enableTracking();

    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/a.scm', line: 1, column: 1 }, env);
    devtools.maybeHit({ filename: 'scheme://app/b.scm', line: 1, column: 1 }, env);

    const history = devtools.getHitHistory();
    assert(logger, 'multi-file hit count', devtools.hitCount, 2);
    assert(logger, 'first hit is file a', history[0], 'scheme://app/a.scm:1:1');
    assert(logger, 'second hit is file b', history[1], 'scheme://app/b.scm:1:1');
  }

  // Test: same line in different files fires both
  {
    const registry = new SchemeSourceRegistry();
    const exprsA = parse('(+ 1 2)\n', { filename: 'scheme://app/a.scm' });
    registry.register('scheme://app/a.scm', '(+ 1 2)\n', 'external', exprsA);
    const exprsB = parse('(+ 3 4)\n', { filename: 'scheme://app/b.scm' });
    registry.register('scheme://app/b.scm', '(+ 3 4)\n', 'external', exprsB);
    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();
    devtools.enableTracking();

    const env = new Environment();

    devtools.maybeHit({ filename: 'scheme://app/a.scm', line: 1, column: 1 }, env);
    devtools.maybeHit({ filename: 'scheme://app/b.scm', line: 1, column: 1 }, env);

    assert(logger, 'location 1:1 in different files fires both', devtools.hitCount, 2);
  }

  // =========================================================================
  // Interpreter Integration - Probes fire during execution
  // =========================================================================
  logger.title('DevToolsDebugIntegration - Interpreter Integration');

  // Test: probe fires during interpreter run() with devtoolsDebug attached
  {
    const { interpreter, env } = createInterpreter();

    const schemeCode = '(+ 1 2)\n';
    const url = 'scheme://app/simple.scm';

    const registry = new SchemeSourceRegistry();
    const exprs = parse(schemeCode, { filename: url });
    registry.register(url, schemeCode, 'external', exprs);

    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();
    devtools.enableTracking();
    interpreter.devtoolsDebug = devtools;

    // Parse with filename so AST nodes carry correct source info
    const expressions = parse(schemeCode, { filename: url });
    const ast = analyze(expressions[0]);
    const result = interpreter.run(ast, env);

    assert(logger, 'interpreter produces correct result', result, 3);
    assert(logger, 'probes fired during execution', devtools.hitCount > 0, true);
  }

  // Test: multi-line code fires probes for multiple lines
  {
    const { interpreter, env } = createInterpreter();

    const schemeCode = '(define x 10)\n(define y 20)\n(+ x y)\n';
    const url = 'scheme://app/multiline.scm';

    const registry = new SchemeSourceRegistry();
    const exprs = parse(schemeCode, { filename: url });
    registry.register(url, schemeCode, 'external', exprs);

    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();
    devtools.enableTracking();
    interpreter.devtoolsDebug = devtools;

    const expressions = parse(schemeCode, { filename: url });
    const ast = analyze(list(intern('begin'), ...expressions));
    const result = interpreter.run(ast, env);

    assert(logger, 'multi-line result is correct', result, 30);
    assert(logger, 'multiple probes fired', devtools.hitCount > 1, true);

    // Verify we see hits from the registered file
    const history = devtools.getHitHistory();
    const fileHits = history.filter(h => h.startsWith('scheme://app/multiline.scm'));
    assert(logger, 'hits come from the registered file', fileHits.length > 0, true);
  }

  // Test: probes fire only when enabled
  {
    const { interpreter, env } = createInterpreter();

    const schemeCode = '(+ 1 2)\n';
    const url = 'scheme://app/disabled.scm';

    const registry = new SchemeSourceRegistry();
    const exprs = parse(schemeCode, { filename: url });
    registry.register(url, schemeCode, 'external', exprs);

    const devtools = new DevToolsDebugIntegration(registry);
    // Intentionally NOT enabling
    devtools.enableTracking();
    interpreter.devtoolsDebug = devtools;

    const expressions = parse(schemeCode, { filename: url });
    const ast = analyze(expressions[0]);
    const result = interpreter.run(ast, env);

    assert(logger, 'result is still correct when disabled', result, 3);
    assert(logger, 'no probes fire when disabled', devtools.hitCount, 0);
  }

  // Test: probes fire during runDebug() as well
  {
    const { interpreter, env } = createInterpreter();

    const schemeCode = '(+ 10 20)\n';
    const url = 'scheme://app/async.scm';

    const registry = new SchemeSourceRegistry();
    const exprs = parse(schemeCode, { filename: url });
    registry.register(url, schemeCode, 'external', exprs);

    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();
    devtools.enableTracking();
    interpreter.devtoolsDebug = devtools;

    const expressions = parse(schemeCode, { filename: url });
    const ast = analyze(expressions[0]);

    // runDebug is async
    (async () => {
      const result = await interpreter.runDebug(ast, env);
      assert(logger, 'runDebug produces correct result', result, 30);
      assert(logger, 'probes fire during runDebug', devtools.hitCount > 0, true);
    })().catch(e => {
      logger.fail(`runDebug probe test threw: ${e.message}`);
    });
  }

  // Test: function call fires probes at function body lines
  {
    const { interpreter, env } = createInterpreter();

    const schemeCode = `(define (add a b)
  (+ a b))
(add 3 4)
`;
    const url = 'scheme://app/function.scm';

    const registry = new SchemeSourceRegistry();
    const exprs = parse(schemeCode, { filename: url });
    registry.register(url, schemeCode, 'external', exprs);

    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();
    devtools.enableTracking();
    interpreter.devtoolsDebug = devtools;

    const expressions = parse(schemeCode, { filename: url });
    const ast = analyze(list(intern('begin'), ...expressions));
    const result = interpreter.run(ast, env);

    assert(logger, 'function call result is correct', result, 7);

    // We should see hits from multiple lines (definition and call and body)
    const history = devtools.getHitHistory();
    const lines = history.map(h => parseInt(h.split(':').pop()));
    const uniqueLines = [...new Set(lines)];
    assert(logger, 'probes fire on multiple lines', uniqueLines.length > 1, true);
  }

  // Test: recursive function fires probes on re-entry (breakpoint re-hit)
  // Uses only JS-level primitives (eq?, -, +) that don't need bootstrap.
  {
    const { interpreter, env } = createInterpreter();

    // countdown: calls itself recursively, returning 0 at base case.
    // Uses only primitives available without bootstrap: eq?, -, +
    const schemeCode = `(define (countdown n)
  (if (eq? n 0)
      0
      (+ 1 (countdown (- n 1)))))
(countdown 5)
`;
    const url = 'scheme://app/countdown.scm';

    const registry = new SchemeSourceRegistry();
    const exprs = parse(schemeCode, { filename: url });
    registry.register(url, schemeCode, 'external', exprs);

    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();
    devtools.enableTracking();
    interpreter.devtoolsDebug = devtools;

    const expressions = parse(schemeCode, { filename: url });
    const ast = analyze(list(intern('begin'), ...expressions));
    const result = interpreter.run(ast, env);

    assert(logger, 'countdown(5) = 5', result, 5);

    // 6 times (n=5,4,3,2,1,0), so the 'if' condition fires at least 6 times.
    const history = devtools.getHitHistory();
    const line2Hits = history.filter(h => h.startsWith('scheme://app/countdown.scm:2'));
    assert(logger, 'line 2 fires for each recursive call (>=6)', line2Hits.length >= 6, true);

    // Note: Line 3 (the literal `0`) does NOT fire a probe because simple
    // literals don't carry source info from the reader. Only compound
    // expressions (lists) get source locations. This is expected — the
    // breakpoint at line 2 (the if) fires for the base case instead.

    // Line 4 (recursive case) should fire for n=5,4,3,2,1 (5 times)
    const line4Hits = history.filter(h => h.startsWith('scheme://app/countdown.scm:4'));
    assert(logger, 'line 4 fires for each recursive case (>=5)', line4Hits.length >= 5, true);
  }

  // =========================================================================
  // evaluateStringDebug Integration (REPL / Test Environment)
  // =========================================================================
  logger.title('DevToolsDebugIntegration - evaluateStringDebug');

  // Test: evaluateStringDebug auto-generates URL and fires probes
  {
    const { interpreter } = createInterpreter();
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();
    devtools.enableTracking();
    interpreter.devtoolsDebug = devtools;

    const code = '(define (times-two x) (* x 2))\n(times-two 21)';
    // Note: No filename passed, evaluateStringDebug should auto-generate one
    // and register it with the sourceRegistry so probes fire.

    // We must await evaluateStringDebug since it is async
    await interpreter.evaluateStringDebug(code);


    // Verify the auto-generated URL was registered
    const url = 'scheme://repl/eval-0.scm';
    assert(logger, 'auto-generated URL is registered', registry.hasSource(url), true);

    // Verify probes were fired at the generated URL
    const history = devtools.getHitHistory();
    const evalHits = history.filter(h => h.startsWith(url));

    // Should fire for definition, call, and function body
    assert(logger, 'probes fired for evaluateStringDebug', evalHits.length >= 3, true);
  }

  // =========================================================================
  // Phase 4: Async Stack Tagging — Task Stack Basics
  // =========================================================================
  logger.title('DevToolsDebugIntegration - Task Stack Basics');

  // Test: taskStack starts empty
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    assert(logger, 'taskStack starts empty', devtools.taskStack.length, 0);
  }

  // Test: onEnterFrame pushes to taskStack
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    devtools.onEnterFrame({ name: 'factorial', originalName: 'factorial', source: null });
    assert(logger, 'taskStack has 1 entry after enter', devtools.taskStack.length, 1);
    assert(logger, 'taskStack entry has name', devtools.taskStack[0].name, 'factorial');
  }

  // Test: onExitFrame pops from taskStack
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    devtools.onEnterFrame({ name: 'foo', source: null });
    devtools.onEnterFrame({ name: 'bar', source: null });
    assert(logger, 'taskStack has 2 entries', devtools.taskStack.length, 2);

    devtools.onExitFrame();
    assert(logger, 'taskStack has 1 after exit', devtools.taskStack.length, 1);
    assert(logger, 'remaining entry is foo', devtools.taskStack[0].name, 'foo');

    devtools.onExitFrame();
    assert(logger, 'taskStack empty after all exits', devtools.taskStack.length, 0);
  }

  // Test: onExitFrame is harmless when stack is empty
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    devtools.onExitFrame(); // Should not throw
    assert(logger, 'exit on empty stack is safe', devtools.taskStack.length, 0);
  }

  // =========================================================================
  // Phase 4: TCO Handling
  // =========================================================================
  logger.title('DevToolsDebugIntegration - TCO Task Replacement');

  // Test: onReplaceFrame replaces top entry
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    devtools.onEnterFrame({ name: 'factorial', originalName: 'factorial', source: null });
    assert(logger, 'initial name is factorial', devtools.taskStack[0].name, 'factorial');

    devtools.onReplaceFrame({ name: 'factorial_loop', originalName: 'loop', source: null });
    assert(logger, 'replaced name is loop', devtools.taskStack[0].name, 'loop');
    assert(logger, 'stack depth unchanged after replace', devtools.taskStack.length, 1);
  }

  // Test: onReplaceFrame uses originalName when available
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    devtools.onEnterFrame({ name: 'a_42', originalName: 'a', source: null });
    devtools.onReplaceFrame({ name: 'b_99', originalName: 'b', source: null });
    assert(logger, 'replace uses originalName', devtools.taskStack[0].name, 'b');
  }

  // Test: onReplaceFrame on empty stack falls back to enter
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    devtools.onReplaceFrame({ name: 'orphan', source: null });
    assert(logger, 'replace on empty stack enters', devtools.taskStack.length, 1);
    assert(logger, 'orphan entry name', devtools.taskStack[0].name, 'orphan');
  }

  // =========================================================================
  // Phase 4: fireProbe Wrapping
  // =========================================================================
  logger.title('DevToolsDebugIntegration - fireProbe');

  // Test: fireProbe calls probe with envProxy
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);
    let calledWith = null;

    const fakeProbe = (ep) => { calledWith = ep; };
    const fakeProxy = { x: 42 };

    devtools.fireProbe(fakeProbe, fakeProxy);
    assert(logger, 'probe called with envProxy', calledWith, fakeProxy);
  }

  // Test: fireProbe wraps in task.run when tasks exist and createTask available
  {
    // Install a mock console.createTask
    const savedCreateTask = console.createTask;
    let taskRunCalled = false;

    console.createTask = (name) => ({
      run: (fn) => { taskRunCalled = true; fn(); }
    });

    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);
    // Force re-detect
    devtools.hasCreateTask = true;

    devtools.onEnterFrame({ name: 'test-fn', source: null });

    let probeCalled = false;
    devtools.fireProbe(() => { probeCalled = true; }, {});

    assert(logger, 'probe was called via fireProbe', probeCalled, true);
    assert(logger, 'task.run was called', taskRunCalled, true);

    // Clean up
    console.createTask = savedCreateTask;
  }

  // Test: fireProbe calls probe directly when no tasks
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);

    let probeCalled = false;
    devtools.fireProbe(() => { probeCalled = true; }, {});

    assert(logger, 'probe called directly without tasks', probeCalled, true);
  }

  // =========================================================================
  // Phase 4: No-op Fallback
  // =========================================================================
  logger.title('DevToolsDebugIntegration - No-op Fallback');

  // Test: when hasCreateTask is false, onEnterFrame still tracks names
  {
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);
    devtools.hasCreateTask = false;

    devtools.onEnterFrame({ name: 'test', source: null });
    // taskStack should still track for fireProbe wrapping reference
    // but task objects will be null
    assert(logger, 'taskStack tracked even without createTask',
      devtools.taskStack.length, 1);
    assert(logger, 'task is null without createTask',
      devtools.taskStack[0].task, null);
  }

  // =========================================================================
  // Phase 4: maybeHit Uses fireProbe
  // =========================================================================
  logger.title('DevToolsDebugIntegration - maybeHit with fireProbe');

  // Test: maybeHit routes through fireProbe
  {
    const code = '(+ 1 2)';
    const { devtools, registry } = createTestDevTools('scheme://app/test.scm', code);

    // Add a task to verify fireProbe wraps correctly
    devtools.onEnterFrame({ name: 'top-level', source: null });

    devtools.enableTracking();
    const env = new Environment();
    env.define('+', (a, b) => a + b);

    devtools.maybeHit(
      { filename: 'scheme://app/test.scm', line: 1, column: 1 },
      env
    );

    assert(logger, 'maybeHit fires probe via fireProbe',
      devtools.hitCount > 0, true);

    devtools.onExitFrame();
  }
}

