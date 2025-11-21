import { Environment } from './environment.js';
import { parse } from './reader.js';
import { analyze } from './analyzer.js';
import { Literal, Variable, If, Let, LetRec, Lambda, TailApp, CallCC, Begin, Executable } from './ast.js';

// --- PART 1: Test Harness ---

/**
 * Helper to run code from strings.
 * @param {Interpreter} interpreter
 * @param {string} code
 * @returns {*}
 */
function run(interpreter, code) {
  // Parse the code, which may contain multiple expressions
  const asts = parse(code);
  if (asts.length === 0) return undefined;

  // If there's more than one, wrap in a 'begin'
  // The parser returns AST nodes, but analyze expects S-exps for lists
  // This is a bit of a mismatch. Let's re-parse for `analyze`
  const raw_asts = new Reader().parse(code); // Use the class directly

  let ast;
  if (raw_asts.length === 1) {
    ast = analyze(raw_asts[0]);
  } else {
    // Construct a 'begin' S-exp and analyze it
    ast = analyze([new Variable('begin'), ...raw_asts]);
  }

  return interpreter.run(ast);
}

// Re-implementing Reader here just for the `run` helper
// This is clumsy, let's fix the `run` helper instead.
class Reader {
  tokenize(code) {
    const regex = /\s*([()]|"(?:[\\].|[^"\\])*"|[^()\s]+)\s*/g;
    const tokens = [];
    let match;
    while ((match = regex.exec(code)) !== null) {
      tokens.push(match[1]);
    }
    return tokens;
  }
  readAtom(token) {
    if (token.startsWith('"')) {
      const strVal = token.substring(1, token.length - 1).replace(/\\"/g, '"').replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\\\/g, '\\');
      return new Literal(strVal);
    }
    const num = parseFloat(token);
    if (!isNaN(num) && num.toString() === token) {
      return new Literal(num);
    }
    if (token === '#t') { return new Literal(true); }
    if (token === '#f') { return new Literal(false); }
    if (token === 'null') { return new Literal(null); }
    return new Variable(token);
  }
  readFromTokens(tokens) {
    if (tokens.length === 0) {
      throw new SyntaxError("Unexpected EOF");
    }
    const token = tokens.shift();
    if (token === '(') {
      const list = [];
      while (tokens[0] !== ')') {
        if (tokens.length === 0) {
          throw new SyntaxError("Missing ')'");
        }
        list.push(this.readFromTokens(tokens));
      }
      tokens.shift();
      return list;
    } else if (token === ')') {
      throw new SyntaxError("Unexpected ')'");
    } else {
      return this.readAtom(token);
    }
  }
  parse(code) {
    const tokens = this.tokenize(code);
    const asts = [];
    while (tokens.length > 0) {
      asts.push(this.readFromTokens(tokens));
    }
    return asts;
  }
}


/**
 * Simple assertion helper
 * @param {object} logger - The logger object.
 * @param {string} description - Test description.
 * @param {*} actual - Actual result.
 * @param {*} expected - Expected result.
 */
function assert(logger, description, actual, expected) {
  // Simple deep equal for arrays
  let isEqual = actual === expected;
  if (Array.isArray(actual) && Array.isArray(expected)) {
    isEqual = actual.length === expected.length &&
      actual.every((val, i) => val === expected[i]);
  }

  if (isEqual) {
    logger.pass(`${description} (Expected: ${expected}, Got: ${actual})`);
  } else {
    logger.fail(`${description} (Expected: ${expected}, Got: ${actual})`);
  }
}

// --- PART 2: Unit Tests ---

/**
 * Runs all unit tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runUnitTests(interpreter, logger) {
  logger.title('Running Unit Tests...');

  // --- Environment Unit Tests ---
  const gEnv = new Environment(null, new Map([['a', 1]]));
  const childEnv = gEnv.extend('b', 2);
  const grandChildEnv = childEnv.extendMany(['c', 'd'], [3, 4]);

  assert(logger, "Unit: env.lookup (global)", gEnv.lookup('a'), 1);
  assert(logger, "Unit: env.lookup (child)", childEnv.lookup('b'), 2);
  assert(logger, "Unit: env.lookup (shadow)", childEnv.extend('a', 10).lookup('a'), 10);
  assert(logger, "Unit: env.lookup (parent)", childEnv.lookup('a'), 1);
  assert(logger, "Unit: env.lookup (grandchild)", grandChildEnv.lookup('c'), 3);
  assert(logger, "Unit: env.lookup (grandparent)", grandChildEnv.lookup('a'), 1);

  try {
    gEnv.lookup('z');
    logger.fail("Unit: env.lookup (unbound) - FAILED to throw");
  } catch (e) {
    assert(logger, "Unit: env.lookup (unbound)", e.message, "Unbound variable: z");
  }

  // Test `set`
  const setEnv = new Environment(null, new Map([['x', 1]]));
  const setChild = setEnv.extend('y', 2);

  setChild.set('y', 20); // Set on self
  assert(logger, "Unit: env.set (self)", setChild.lookup('y'), 20);
  assert(logger, "Unit: env.set (self, parent unchanged)", setEnv.lookup('x'), 1);

  setChild.set('x', 10); // Set on parent
  assert(logger, "Unit: env.set (parent)", setEnv.lookup('x'), 10);
  assert(logger, "Unit: env.set (parent, child lookup)", setChild.lookup('x'), 10);

  setChild.set('z', 99); // Set on global (not found)
  assert(logger, "Unit: env.set (global)", setEnv.lookup('z'), 99);

  // --- Parser Unit Tests ---
  logger.title('Running Parser Unit Tests...');
  try {
    let ast = parse("123")[0];
    assert(logger, "Unit: parse number", ast instanceof Literal && ast.value === 123, true);
    ast = parse("foo")[0];
    assert(logger, "Unit: parse symbol", ast instanceof Variable && ast.name === "foo", true);
    ast = parse("#t")[0];
    assert(logger, "Unit: parse bool #t", ast instanceof Literal && ast.value === true, true);
    ast = parse("#f")[0];
    assert(logger, "Unit: parse bool #f", ast instanceof Literal && ast.value === false, true);
    ast = parse(`"hello"`)[0];
    assert(logger, "Unit: parse string", ast instanceof Literal && ast.value === "hello", true);

    // String Escaping Tests
    ast = parse(`"\\n"`)[0];
    assert(logger, "Unit: parse string \\n", ast.value, "\n");
    ast = parse(`"\\""`)[0];
    assert(logger, "Unit: parse string \\\"", ast.value, "\"");
    ast = parse(`"\\\\"`)[0];
    assert(logger, "Unit: parse string \\\\", ast.value, "\\");


    const list = parse("(+ 1 2)")[0];
    assert(logger, "Unit: parse simple list (tag)", list[0] instanceof Variable && list[0].name === "+", true);
    assert(logger, "Unit: parse simple list (arg1)", list[1] instanceof Literal && list[1].value === 1, true);

    assert(logger, "Unit: parse multiple exprs", parse("1 2")[1] instanceof Literal, true);
  } catch (e) {
    logger.fail(`Parser unit tests failed: ${e.message}`);
  }

  // --- Reader Error Tests ---
  logger.title('Running Reader Error Tests...');
  try {
    try {
      parse(")");
      logger.fail("Reader: Unexpected ')' - FAILED to throw");
    } catch (e) {
      assert(logger, "Reader: Unexpected ')'", e.message, "Unexpected ')'");
    }

    try {
      parse("(+ 1 2");
      logger.fail("Reader: Missing ')' - FAILED to throw");
    } catch (e) {
      assert(logger, "Reader: Missing ')'", e.message, "Missing ')'");
    }

    try {
      parse("");
      // Depending on implementation, empty string might return [] or throw EOF
      // The current implementation throws "Unexpected EOF" if tokens are empty but readFromTokens is called?
      // Actually, parse() loops while tokens > 0. If empty, it returns [].
      // Let's test a case that causes EOF during read, like "("
      parse("(");
      logger.fail("Reader: Unexpected EOF - FAILED to throw");
    } catch (e) {
      assert(logger, "Reader: Unexpected EOF", e.message, "Missing ')'");
    }

  } catch (e) {
    logger.fail(`Reader error tests failed: ${e.message}`);
  }

  // --- Analyzer Unit Tests ---
  logger.title('Running Analyzer Unit Tests...');
  try {
    assert(logger, "Unit: analyze literal", analyze(new Literal(1)) instanceof Literal, true);
    assert(logger, "Unit: analyze variable", analyze(new Variable("x")) instanceof Variable, true);

    let ast = analyze(parse(`(if #t 1 2)`)[0]);
    assert(logger, "Unit: analyze if", ast instanceof If, true);
    ast = analyze(parse(`(call/cc (lambda (k) k))`)[0]);
    assert(logger, "Unit: analyze call/cc", ast instanceof CallCC, true);
    ast = analyze(parse(`(let ((x 1)) x)`)[0]);
    assert(logger, "Unit: analyze let", ast instanceof Let, true);
    ast = analyze(parse(`(letrec ((f (lambda () 0))) (f))`)[0]);
    assert(logger, "Unit: analyze letrec", ast instanceof LetRec, true);
    ast = analyze(parse(`(lambda (x) x)`)[0]);
    assert(logger, "Unit: analyze lambda (single body)", ast instanceof Lambda, true);
    ast = analyze(parse(`(+ 1 2)`)[0]);
    assert(logger, "Unit: analyze app", ast instanceof TailApp, true);
    ast = analyze(parse(`(begin 1 2)`)[0]);
    assert(logger, "Unit: analyze begin", ast instanceof Begin, true);

    // Test lambda with multiple body expressions
    const lambdaMultiBody = analyze(parse(`(lambda (x) 1 x)`)[0]);
    assert(logger, "Unit: analyze lambda (multi-body)", lambdaMultiBody.body instanceof Begin, true);
  } catch (e) {
    logger.fail(`Analyzer unit tests failed: ${e.message}`);
  }

  // --- Analyzer Error Tests ---
  logger.title('Running Analyzer Error Tests...');
  try {
    try {
      analyze(parse("(lambda (x))")[0]);
      logger.fail("Analyzer: Malformed lambda - FAILED to throw");
    } catch (e) {
      // The error message might vary, checking for "Malformed lambda" or similar
      const msg = e.message;
      assert(logger, "Analyzer: Malformed lambda", msg.includes("Malformed lambda"), true);
    }

    // Test (if) with missing args - The current analyzer might not check arg count strictly for all forms,
    // but let's see what happens. If it doesn't throw, we might want to add a check or just document it.
    // Looking at analyzer.js: return new If(analyze(exp[1]), analyze(exp[2]), analyze(exp[3]));
    // If exp[3] is missing, analyze(undefined) throws "received 'undefined' expression".
    try {
      analyze(parse("(if #t 1)")[0]);
      logger.fail("Analyzer: Malformed if - FAILED to throw");
    } catch (e) {
      assert(logger, "Analyzer: Malformed if", e.message.includes("undefined"), true);
    }

  } catch (e) {
    logger.fail(`Analyzer error tests failed: ${e.message}`);
  }
}

// --- PART 3: Functional Tests ---

/**
 * Runs all functional tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export async function runAllTests(interpreter, logger) {

  logger.title("Basic Evaluation & Native Calls");

  let result = run(interpreter, `(+ 2 3)`);
  assert(logger, "Native '+' call", result, 5);

  result = run(interpreter, `(let ((x 10)) (* x 2))`);
  assert(logger, "'let' binding", result, 20);

  result = run(interpreter, `(if (> 10 5) "yes" "no")`);
  assert(logger, "'if' expression (true)", result, 'yes');

  // --- TCO Test ---
  logger.title("Tail Call Optimization (TCO)");

  const tcoTest = `
        (letrec ((loop (lambda (n acc)
                         (if (= n 0)
                             acc
                             (loop (- n 1) (+ acc n))))))
          (loop 5 0))`;

  result = run(interpreter, tcoTest);
  assert(logger, "TCO sum (1-5)", result, 15);

  const tcoLargeTest = `
        (letrec ((loop (lambda (n acc)
                         (if (= n 0)
                             acc
                             (loop (- n 1) (+ acc n))))))
          (loop 5000 0))`;

  try {
    result = run(interpreter, tcoLargeTest);
    assert(logger, "TCO deep recursion (n=5000)", result, 12502500);
  } catch (e) {
    logger.fail(`TCO deep recursion (n=5000) failed: ${e.message}`);
  }

  // --- Call/CC Tests ---
  logger.title("First-Class Continuations (call/cc)");

  result = run(interpreter, `
        (let ((val (+ 1 (call/cc (lambda (k) (k 10))))))
          val)`);
  assert(logger, "call/cc simple return", result, 11);

  result = run(interpreter, `
        (let ((val (+ 1 (call/cc (lambda (k) 10)))))
          val)`);
  assert(logger, "call/cc capture but don't use", result, 11);

  // --- call/cc (Capture and Invoke) ---
  logger.title("call/cc (Capture and Invoke)");

  run(interpreter, `(set! k-holder #f)`);

  result = run(interpreter, `
        (let ((val (+ 10 (call/cc (lambda (k)
                                   (begin
                                     (set! k-holder k)
                                     5))))))
          val)`);
  assert(logger, "call/cc capture and save", result, 15);

  result = run(interpreter, `(k-holder 20)`);
  assert(logger, "call/cc invoke saved continuation", result, 30);

  run(interpreter, `(set! foo (lambda () (k-holder 50)))`);

  result = run(interpreter, `(let ((x 1000)) (foo))`);
  assert(logger, "call/cc invoke from deep stack", result, 60);

  // --- Async Callback Test ---
  logger.title("Native Async Callback (JS -> Scheme)");

  run(interpreter, `
        (js-set-timeout (lambda () (display "Async callback finished!")) 
                        100)`);
  logger.log("Scheduled async callback... (check console for 'DISPLAY: Async callback finished!')", 'info');


  // --- JS Interop Tests ---

  // Globally define 'loop' for TCO interop test
  run(interpreter, `
        (set! loop (lambda (n acc)
                     (if (= n 0)
                         acc
                         (loop (- n 1) (+ acc n)))))`);

  logger.title("call/cc (JS Interop)");

  // Test 1: JS stores k, then JS invokes k
  run(interpreter, `(set! final-result "interop-test-1")`);

  result = run(interpreter, `
        (let ((val (+ 100 (call/cc (lambda (k)
                                    (begin
                                      (js-store-k k)
                                      5))))))
          (set! final-result val))`);
  assert(logger, "JS Interop: Store k (initial run)", result, 105);

  logger.log("JS Interop: Invoking k from JS...", 'info');
  run(interpreter, `(js-invoke-k-from-js 25)`);

  result = run(interpreter, `final-result`);
  assert(logger, "JS Interop: Result after JS invokes k", result, 125); // 100 + 25

  // Test 2: Capture k *inside* a JS-initiated callback
  run(interpreter, `(set! callback-k #f)`);
  run(interpreter, `
        (js-run-callback
         (lambda ()
           (let ((val (+ 1000 (call/cc (lambda (k)
                                         (begin
                                           (set! callback-k k)
                                           10))))))
             (set! final-result val))))`);

  result = run(interpreter, `final-result`);
  assert(logger, "JS Interop: Capture k inside JS callback", result, 1010);

  run(interpreter, `(callback-k 30)`);
  result = run(interpreter, `final-result`);
  assert(logger, "JS Interop: Invoke k captured in callback", result, 1030); // 1000 + 30

  // Test 3: TCO from JS-initiated callback
  run(interpreter, `(set! final-result "tco-test-not-run")`);
  try {
    run(interpreter, `
            (js-run-callback
             (lambda ()
               (set! final-result (loop 5000 0))))`);

    result = run(interpreter, `final-result`);
    assert(logger, "JS Interop: TCO from JS callback (n=5000)", result, 12502500);
  } catch (e) {
    logger.fail(`JS Interop: TCO from JS callback (n=5000) failed: ${e.message}`);
  }


  // --- Async call/cc Interoperability Test ---
  logger.title("Async call/cc Interoperability");

  logger.log("Starting async cancellation test...", 'info');

  run(interpreter, `(set! final-result "not-run")`);
  run(interpreter, `(set! cancel-k #f)`);

  run(interpreter, `
        (set! cancel
          (lambda (reason)
            (if cancel-k
                (let ((k cancel-k))
                  (begin
                    (set! cancel-k #f)
                    (k reason)))
                (display "no-k"))))`);

  run(interpreter, `
        (set! final-result
          (call/cc
           (lambda (k)
             (begin
               (set! cancel-k k)
               (js-fetch-data
                (lambda (data)
                  (begin
                    (if cancel-k
                        (let ((k cancel-k))
                          (begin
                            (set! cancel-k #f)
                            (k (list "success" data))))
                        (display "Data callback was cancelled"))
                    (newline))))
               "waiting"))))`);

  let immediateResult = run(interpreter, `final-result`);
  assert(logger, "Async: Initial result", immediateResult, 'waiting');

  logger.log("Simulating user cancellation...", 'info');
  run(interpreter, `(cancel "Cancelled by user")`);

  let cancelledResult = run(interpreter, `final-result`);
  assert(logger, "Async: Result after cancellation", cancelledResult, "Cancelled by user");

  await new Promise(resolve => setTimeout(resolve, 2000));

  logger.log("Async: Checking final result after 2s...", 'info');
  let finalResult = run(interpreter, `final-result`);
  assert(logger, "Async: Final result (cancellation wins)", finalResult, "Cancelled by user");

  // This log will appear *after* the async callbacks have fired,
  // because the 'await' above pauses this function.
  logger.log("...Async test assertions scheduled.", 'info');

  // --- Runtime Error Tests ---
  logger.title("Runtime Error Tests");

  try {
    run(interpreter, `(1 2)`);
    logger.fail("Runtime: Apply non-function - FAILED to throw");
  } catch (e) {
    assert(logger, "Runtime: Apply non-function", e.message.includes("Not a function"), true);
  }

  try {
    run(interpreter, `(undefined-var)`);
    logger.fail("Runtime: Unbound variable - FAILED to throw");
  } catch (e) {
    assert(logger, "Runtime: Unbound variable", e.message.includes("Unbound variable"), true);
  }

  // --- Edge Case Tests ---
  logger.title("Edge Case Tests");

  result = run(interpreter, `(begin)`);
  assert(logger, "Edge: Empty begin", result, null);

  result = run(interpreter, `(if #t 1 2)`);
  assert(logger, "Edge: If #t", result, 1);

  result = run(interpreter, `(if #f 1 2)`);
  assert(logger, "Edge: If #f", result, 2);

  // set! returns the value set
  result = run(interpreter, `(let ((x 1)) (set! x 2))`);
  assert(logger, "Edge: set! return value", result, 2);

  // letrec self-reference (limitation: returns null instead of error)
  result = run(interpreter, `(letrec ((x x)) x)`);
  assert(logger, "Edge: letrec self-reference", result, null);


  // --- Continuation Arity Tests ---
  logger.title("Continuation Arity Tests");

  // Calling k with no args -> null
  result = run(interpreter, `(call/cc (lambda (k) (k)))`);
  assert(logger, "Continuation: 0 args", result, null);

  // Calling k with multiple args -> first arg used
  result = run(interpreter, `(call/cc (lambda (k) (k 1 2 3)))`);
  assert(logger, "Continuation: multiple args", result, 1);

  // --- JS Interop Error Tests ---
  logger.title("JS Interop Error Tests");

  // We need a function that throws. We can use a lambda that calls a non-existent JS function 
  // or we can add a throwing primitive if we could, but we can't easily modify env here without access.
  // However, we can use 'js-run-callback' to run code that throws? No, that runs scheme code.
  // Let's try to invoke a method on a null object or similar via some trick, 
  // OR we can rely on the fact that we can't easily add a throwing native function without modifying environment.js.
  // Wait, we can define a new primitive in the global env if we have access to it?
  // The interpreter has .globalEnv.

  if (interpreter.globalEnv) {
    // Add a throwing native function
    interpreter.globalEnv.bindings.set('js-throw', new (interpreter.globalEnv.lookup('+').constructor)(
      () => { throw new Error("Boom from JS"); },
      interpreter
    ));

    try {
      run(interpreter, `(js-throw)`);
      logger.fail("JS Interop: Throw - FAILED to throw");
    } catch (e) {
      assert(logger, "JS Interop: Throw", e.message, "Boom from JS");
    }
  } else {
    logger.log("Skipping JS Interop Error test (no globalEnv access)", 'warn');
  }


}
