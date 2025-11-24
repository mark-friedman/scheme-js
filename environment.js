import { NativeJsFunction, Literal, TailApp } from './ast.js';

/**
 * Manages lexical scope via a chain of maps.
 */
export class Environment {
  /**
   * @param {Environment | null} parent - The parent environment.
   * @param {Map<string, *>} [bindings] - Local bindings for this frame.
   */
  constructor(parent = null, bindings = new Map()) {
    this.parent = parent;
    this.bindings = bindings;
  }

  /**
   * Creates a new child environment.
   * @param {string} name - The variable name to bind.
   * @param {*} value - The value to bind.
   * @returns {Environment} A new child environment.
   */
  extend(name, value) {
    return new Environment(this, new Map([[name, value]]));
  }

  /**
   * Creates a new child environment with multiple bindings.
   * @param {Array<string>} names - The variable names.
   * @param {Array<*>} values - The corresponding values.
   * @returns {Environment} A new child environment.
   */
  extendMany(names, values) {
    const newBindings = new Map();
    for (let i = 0; i < names.length; i++) {
      newBindings.set(names[i], values[i]);
    }
    return new Environment(this, newBindings);
  }

  /**
   * Finds a variable's value by searching up the scope chain.
   * @param {string} name - The variable name to look up.
   * @returns {*} The bound value.
   */
  lookup(name) {
    if (this.bindings.has(name)) {
      return this.bindings.get(name);
    }
    if (this.parent) {
      return this.parent.lookup(name);
    }
    throw new Error(`Unbound variable: ${name}`);
  }

  /**
   * Finds the environment where a variable is defined.
   * @param {string} name - The variable name.
   * @returns {Environment | null} The environment, or null if not found.
   */
  findEnv(name) {
    if (this.bindings.has(name)) {
      return this;
    }
    if (this.parent) {
      return this.parent.findEnv(name);
    }
    return null;
  }

  /**
   * Updates an existing variable in the scope chain.
   * @param {string} name - The variable name.
   * @param {*} value - The new value.
   * @returns {*} The new value.
   */
  set(name, value) {
    const env = this.findEnv(name);
    if (!env) {
      // Set at the *top* (global) level.
      let top = this;
      while (top.parent) { top = top.parent; }
      top.bindings.set(name, value);
    } else {
      env.bindings.set(name, value);
    }
    return value;
  }

  /**
   * Defines a variable in the *current* environment frame.
   * Shadows any outer bindings of the same name.
   * @param {string} name - The variable name.
   * @param {*} value - The value to bind.
   * @returns {*} The value.
   */
  define(name, value) {
    this.bindings.set(name, value);
    return value;
  }
}


/**
 * A simple JavaScript function that simulates an async operation.
 * We attach it to `window` so it's in the global JS scope.
 */
window.fetchData = (successCallback) => {
  setTimeout(() => {
    console.log("JavaScript is executing the callback.");
    // The successCallback is a Scheme closure wrapped by NativeJsFunction
    successCallback("Fetched data from JS");
  }, 1000); // 1 second delay
}

/**
 * A global JS store for holding a continuation, for testing.
 */
window.globalK = null;

/**
 * Creates the global environment with built-in primitives.
 * @param {Interpreter} interpreter - A reference to the interpreter for async callbacks.
 * @returns {Environment} The global environment.
 */
export function createGlobalEnvironment(interpreter) {
  // Helper to wrap native JS functions
  const wrap = (fn) => new NativeJsFunction(fn, interpreter);

  const bindings = new Map([
    // Math
    ['+', wrap((a, b) => a + b)],
    ['-', wrap((a, b) => a - b)],
    ['*', wrap((a, b) => a * b)],
    ['/', wrap((a, b) => a / b)],
    // Comparison
    ['=', wrap((a, b) => a === b)],
    ['<', wrap((a, b) => a < b)],
    ['>', wrap((a, b) => a > b)],
    // Lists (as simple JS arrays)
    ['cons', wrap((a, b) => [a, b])],
    ['car', wrap((p) => p[0])],
    ['cdr', wrap((p) => p[1])],
    ['list', wrap((...args) => args)],
    ['null?', wrap((a) => a === null)], // Using null for '()
    ['append', wrap((...args) => {
      if (args.length === 0) return null;
      let res = args[0];
      for (let i = 1; i < args.length; i++) {
        const val = args[i];
        if (res === null) {
          res = val;
        } else if (val !== null) {
          res = res.concat(val);
        }
      }
      return res;
    })],
    // I/O
    ['display', wrap((val) => {
      // In REPL, we might want to buffer this
      console.log("DISPLAY:", val);
      return val;
    })],
    ['newline', wrap(() => {
      console.log("");
      return true; // Return a truthy value
    })],

    // --- JS Interop Test Functions ---
    ['js-set-timeout', wrap((cb, ms) => setTimeout(cb, ms))],
    ['js-fetch-data', wrap(window.fetchData)],

    // Stores a value (e.g., a continuation) in a global JS variable.
    ['js-store-k', wrap((k) => {
      window.globalK = k; // k will be a Continuation object
      return true;
    })],

    // Simulates a JS event invoking a stored continuation.
    ['js-invoke-k-from-js', wrap((val) => {
      if (window.globalK) {
        const k = window.globalK;
        // To invoke the continuation from JS, we must
        // start a *new* interpreter run that
        // has (k val) as its AST.
        const ast = new TailApp(new Literal(k), [new Literal(val)]);
        // This run will "jump" stacks.
        interpreter.run(ast, interpreter.globalEnv);
        return true;
      }
      console.error("JS: No k stored to invoke!");
      return false;
    })],

    // Runs a Scheme callback from a native context.
    ['js-run-callback', wrap((cb) => {
      // cb is a Scheme Closure. The wrapper
      // in NativeJsFunction.call will turn it
      // into a callable JS function.
      cb(); // This cb() is the JS wrapper which calls interpreter.run
      return true;
    })],
  ]);

  return new Environment(null, bindings);
}
