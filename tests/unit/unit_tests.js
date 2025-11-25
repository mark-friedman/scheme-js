import { Environment } from '../../src/core/environment.js';
import { parse } from '../../src/syntax/reader.js';
import { analyze } from '../../src/syntax/analyzer.js';
import { prettyPrint } from '../../web/repl.js';
import { Literal, Variable, If, Let, LetRec, Lambda, TailApp, CallCC, Begin } from '../../src/syntax/ast.js';
import { assert, createTestLogger, createTestEnv } from '../helpers.js';

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

        try {
            analyze(parse("(if #t 1)")[0]);
            logger.fail("Analyzer: Malformed if - FAILED to throw");
        } catch (e) {
            assert(logger, "Analyzer: Malformed if", e.message.includes("undefined"), true);
        }

    } catch (e) {
        logger.fail(`Analyzer error tests failed: ${e.message}`);
    }

    // --- REPL Unit Tests ---
    logger.title('Running REPL Unit Tests...');
    try {
        assert(logger, "Unit: prettyPrint symbol", prettyPrint(new Variable("x")), "x");
        assert(logger, "Unit: prettyPrint number", prettyPrint(123), "123");
        assert(logger, "Unit: prettyPrint list", prettyPrint([new Literal(1), new Variable("a")]), "'(1 a)");
    } catch (e) {
        logger.fail(`REPL unit tests failed: ${e.message}`);
    }
}

// Allow running directly via node
// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runUnitTests(interpreter, logger);
}
