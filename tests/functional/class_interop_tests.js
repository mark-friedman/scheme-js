import { assert, run, createTestLogger, createTestEnv } from '../harness/helpers.js';

/**
 * Runs Class Interop tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runClassInteropTests(interpreter, logger) {
  logger.title("Class Interop and 'this' Binding");

  // 1. Defining a class in Scheme and using it in JS
  run(interpreter, `
        (define-class <Person>
          (Person name age)
          person?
          (fields (name person-name) (age person-age))
          (methods
            (greet (other) (string-append "Hello " other ", I am " this.name))))
    `);

  const Person = run(interpreter, "Person");
  assert(logger, "Scheme class is a JS constructor", typeof Person, 'function');

  const alice = new Person("Alice", 30);
  assert(logger, "Instance has correct properties", alice.name, "Alice");
  assert(logger, "Instance has correct age", alice.age, 30);

  const greeting = alice.greet("Bob");
  assert(logger, "Method call from JS works and binds 'this'", greeting, "Hello Bob, I am Alice");

  // 2. Testing 'bind' on Scheme methods
  const boundGreet = alice.greet.bind({ name: "Fake Alice" });
  const boundGreeting = boundGreet("Bob");
  assert(logger, "'bind' works on Scheme methods", boundGreeting, "Hello Bob, I am Fake Alice");

  // 3. Testing 'bind' on plain Scheme closures
  run(interpreter, `
        (define (simple-greet other)
          (string-append "Hi " other ", I'm " this.name))
    `);
  const simpleGreet = run(interpreter, "simple-greet");
  const boundSimple = simpleGreet.bind({ name: "Charlie" });
  assert(logger, "'bind' works on plain Scheme closures", boundSimple("Dave"), "Hi Dave, I'm Charlie");

  // 4. Testing 'bind' with multiple arguments (partial application)
  run(interpreter, `(define (add-three a b c) (+ a b c))`);
  const addThree = run(interpreter, "add-three");
  const addTwo = addThree.bind(null, 10);
  assert(logger, "'bind' partial application (1 arg)", addTwo(20, 30), 60);

  const addOne = addThree.bind(null, 10, 20);
  assert(logger, "'bind' partial application (2 args)", addOne(30), 60);

  const boundMethod = alice.greet.bind({ name: "Eve" }, "Frank");
  assert(logger, "'bind' partial application on method", boundMethod(), "Hello Frank, I am Eve");

  // 5. Testing 'bind' on continuations
  let caughtVals = null;
  let capturedK = null;
  interpreter.globalEnv.define('capture-vals', (vals) => { caughtVals = vals; });
  interpreter.globalEnv.define('capture-k', (k) => { capturedK = k; });
  run(interpreter, `
    (call-with-values 
      (lambda () 
        (call/cc (lambda (k) (capture-k k) 100)))
      (lambda args (capture-vals args)))
  `);

  const boundK = capturedK.bind(null, 10);
  boundK(20, 30);
  assert(logger, "'bind' works on continuations (partial app)", caughtVals, [10, 20, 30]);

  // 6. Verification of dot notation expansion in Scheme
  // passing JS object to Scheme
  interpreter.globalEnv.define('js-test-obj', {
    val: 100,
    add: function (x) { return this.val + x; }
  });

  const result = run(interpreter, "(js-test-obj.add 50)");
  assert(logger, "Dot notation expands to js-invoke for method call", result, 150);

  // 7. Inheritance check in JS
  run(interpreter, `
        (define-class <Employee> <Person>
          (Employee name age job)
          employee?
          (fields (job employee-job))
          (methods
            (work () (string-append this.name " is working as " this.job))))
    `);

  const Employee = run(interpreter, "Employee");
  const eve = new Employee("Eve", 28, "Engineer");
  assert(logger, "Subclass instance of Person", eve instanceof Person, true);
  assert(logger, "Subclass method call", eve.work(), "Eve is working as Engineer");
  assert(logger, "Subclass calling inherited method", eve.greet("Alice"), "Hello Alice, I am Eve");
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
  const { interpreter } = createTestEnv();
  const logger = createTestLogger();
  runClassInteropTests(interpreter, logger);
}
