import { assert, run } from '../helpers.js';

export async function runRecordInteropTests(interpreter, logger, fileLoader) {
    logger.title('Running Record Interop Tests...');

    // 0. Reader Test: Dotted List
    try {
        run(interpreter, `(quote (a . b))`);
        logger.log("Reader: Dotted list parsed successfully", 'info');
    } catch (e) {
        logger.fail(`Reader: Dotted list failed: ${e.message}`);
    }

    // 1. Primitive: make-record-type
    try {
        // (make-record-type "Point" '(x y)) -> Returns the Class constructor (RTD)
        // We also need to define it in the environment to use it.
        run(interpreter, `
            (define PointRTD (make-record-type "Point" '(x y)))
        `);

        // Check if PointRTD is a function (Class constructor)
        const PointRTD = interpreter.globalEnv.lookup('PointRTD');
        assert(logger, "make-record-type returns a function (Class)", typeof PointRTD, 'function');
        assert(logger, "Class name is Point", PointRTD.name, 'Point');

        // 2. Primitive: record-constructor
        // (record-constructor PointRTD) -> Returns a constructor function
        run(interpreter, `
            (define make-point (record-constructor PointRTD))
        `);
        const makePoint = interpreter.globalEnv.lookup('make-point');
        assert(logger, "record-constructor returns a function", typeof makePoint, 'function');

        // Create an instance
        const p1 = run(interpreter, `(make-point 10 20)`);
        assert(logger, "Instance created", typeof p1, 'object');
        assert(logger, "Instance is instanceof Point", p1 instanceof PointRTD, true);

        // 3. Primitive: record-predicate
        run(interpreter, `
            (define point? (record-predicate PointRTD))
        `);
        assert(logger, "point? true for instance", run(interpreter, `(point? (make-point 1 2))`), true);
        assert(logger, "point? false for other", run(interpreter, `(point? "not-a-point")`), false);

        // 4. Primitive: record-accessor
        run(interpreter, `
            (define point-x (record-accessor PointRTD 'x))
            (define point-y (record-accessor PointRTD 'y))
        `);
        assert(logger, "accessor x", run(interpreter, `(point-x (make-point 10 20))`), 10);
        assert(logger, "accessor y", run(interpreter, `(point-y (make-point 10 20))`), 20);

        // 5. Primitive: record-modifier
        run(interpreter, `
            (define point-x-set! (record-modifier PointRTD 'x))
        `);
        run(interpreter, `
            (define p2 (make-point 100 200))
            (point-x-set! p2 999)
        `);
        assert(logger, "modifier updates value", run(interpreter, `(point-x p2)`), 999);

        // 6. Interop: JS access
        // We can access fields directly in JS
        const p3 = run(interpreter, `(make-point 5 5)`);
        assert(logger, "JS property access x", p3.x, 5);
        assert(logger, "JS property access y", p3.y, 5);

    } catch (e) {
        logger.fail(`Record Primitives failed: ${e.message}`);
    }

    // 7. Macro: define-record-type
    try {
        // Load macros.scm to get the define-record-type macro
        // (The macros were split out from core.scm into separate files)
        let macrosCode;
        if (fileLoader) {
            macrosCode = await fileLoader('src/core/scheme/macros.scm');
        } else {
            // Fallback for direct node execution (less robust but works for debugging)
            const fs = await import('fs');
            const path = await import('path');
            const macrosPath = path.join(process.cwd(), 'src', 'core', 'scheme', 'macros.scm');
            try {
                macrosCode = fs.readFileSync(macrosPath, 'utf8');
            } catch (e) {
                logger.fail("Cannot load macros.scm: No file loader provided");
                return;
            }
        }
        run(interpreter, macrosCode);

        run(interpreter, `
            (define-record-type Node
              (make-node val next)
              node?
              (val node-val)
              (next node-next set-node-next!))
        `);

        run(interpreter, `(define n1 (make-node "A" '()))`);
        assert(logger, "Macro: constructor", run(interpreter, `(node? n1)`), true);
        assert(logger, "Macro: accessor", run(interpreter, `(node-val n1)`), "A");

        run(interpreter, `(set-node-next! n1 "B")`);
        assert(logger, "Macro: modifier", run(interpreter, `(node-next n1)`), "B");

        // Interop check
        const n1 = interpreter.globalEnv.lookup('n1');
        assert(logger, "Macro: JS class name", n1.constructor.name, "Node");
        assert(logger, "Macro: JS property val", n1.val, "A");

    } catch (e) {
        logger.fail(`define-record-type Macro failed: ${e.message}`);
    }
}
