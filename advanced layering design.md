Scheme R7RS-Small on JavaScript: Layered Architecture Plan

Version: 1.0
Objective: To build a compliant Scheme interpreter through incremental, testable layers, ensuring separation of concerns between the JavaScript kernel and the Scheme runtime.

1. The Core Philosophy

We treat the system not as a single monolithic codebase, but as a chain of configurations.

JavaScript Layering: Implemented via a "Chain of Factories." Each layer exports a setup function that builds upon the previous layer's instance.

Scheme Layering: Implemented via a "Bootstrap Library System." Scheme code is organized into libraries that explicitly declare dependencies on other Scheme libraries or native JavaScript modules.

2. Directory Structure

The source code is organized into numbered directories. This makes the dependency hierarchy explicit: higher numbers depend on lower numbers, never the reverse.

/project-root
  ├── package.json
  ├── README.md
  │
  ├── src/
  │   │
  │   ├── layer-1-kernel/         # THE KERNEL
  │   │   ├── index.js            # EXPORT: createLayer1()
  │   │   ├── interpreter.js      # The core trampoline loop
  │   │   ├── ast.js              # Base AST nodes
  │   │   ├── library.js          # The "Micro-Library" registry
  │   │   └── primitives.js       # Base JS natives (+, display, etc.)
  │   │
  │   ├── layer-2-syntax/         # SYNTACTIC ABSTRACTION
  │   │   ├── index.js            # EXPORT: createLayer2()
  │   │   ├── expander.js         # JS logic for syntax-rules
  │   │   └── syntax.scm          # (define-library (scheme syntax) ...)
  │   │
  │   ├── layer-3-data/           # DATA STRUCTURES
  │   │   ├── index.js            # EXPORT: createLayer3()
  │   │   ├── cons.js             # JS class for Pairs
  │   │   ├── symbol.js           # JS class for Interned Symbols
  │   │   └── data.scm            # (define-library (scheme data) ...)
  │   │
  │   └── layer-4-stdlib/         # STANDARD LIBRARY
  │       ├── index.js            # EXPORT: createLayer4()
  │       └── base.scm            # (define-library (scheme base) ...)
  │
  ├── tests/
  │   ├── runner.js               # Universal Test Runner
  │   ├── layer-1/                # Tests for Kernel only
  │   ├── layer-2/                # Tests for Macros
  │   └── layer-3/                # Tests for Cons/Lists
  │
  └── web/
      ├── index.html
      └── main.js                 # UI Entry point (targets latest layer)



3. JavaScript Mechanism: The Chain of Factories

To ensure layers are separately runnable, we do not simply modify a global Interpreter class. Instead, each layer exports a Factory Function that constructs a valid interpreter for that specific stage of development.

Layer 1 Factory (src/layer-1-kernel/index.js)

This layer builds the foundation from scratch.

import { Interpreter } from './interpreter.js';
import { createGlobalEnvironment } from './primitives.js';
import { LibraryRegistry } from './library.js';

export function createLayer1() {
    const interpreter = new Interpreter();
    const env = createGlobalEnvironment(interpreter);
    
    // Initialize the Micro-Library system
    interpreter.libraries = new LibraryRegistry();
    
    interpreter.setGlobalEnv(env);
    return { interpreter, env };
}



Layer 2 Factory (src/layer-2-syntax/index.js)

This layer imports Layer 1, instantiates it, and then mutates it to add new capabilities.

import { createLayer1 } from '../layer-1-kernel/index.js';
import { expandMacros } from './expander.js';

export function createLayer2() {
    // 1. Build the previous layer
    const { interpreter, env } = createLayer1();

    // 2. Monkey-patch / Decorate the analyzer to support macros
    const baseAnalyze = interpreter.analyze; 
    interpreter.analyze = (sexp) => {
        // Pre-processing step for macros before standard analysis
        const expanded = expandMacros(sexp, env); 
        return baseAnalyze.call(interpreter, expanded);
    };

    // 3. Load Scheme-side syntax definitions
    // (This uses the import-native feature defined below)
    interpreter.eval('(import-native "src/layer-2-syntax/syntax.scm")');

    return { interpreter, env };
}



4. Scheme Mechanism: The Bootstrap Library System

To manage Scheme code layering, we implement a subset of R7RS define-library in Layer 1. This allows us to write standard-looking Scheme code that handles dependencies.

The "Micro-Library" Syntax

We support three specific forms in the Kernel analyzer:

(define-library (name ...) decls...): Defines a new scope.

(import (name ...)): Copies bindings from a registered library into the current scope.

(import-native "path/to/file.js"): Dynamically loads a JS module and calls its default export (or specific install function) to extend the environment.

Layer 2 Scheme Example

Layer 2 can now be defined as a Scheme library that depends on native JS code:

;; src/layer-2-syntax/syntax.scm
(define-library (scheme syntax)
  ;; Load the JS expander logic into the environment
  (import-native "./expander.js") 
  
  (export define-syntax let-syntax ...)
  
  (begin
     ;; Scheme helper code for macros...
  ))



Layer 3 Scheme Example

Layer 3 depends on Layer 2 via standard import:

;; src/layer-3-data/data.scm
(define-library (scheme data)
  (import (scheme syntax)) ;; Access to define-syntax
  (import-native "./cons.js") ;; Native Cons implementation
  
  (export cons car cdr map ...)
  
  (begin
    (define (map proc list) ... ) ;; Implemented using macros from L2
  ))



5. The Universal Test Runner

We need a single entry point (tests/runner.js) that can target any layer. This ensures that as we break things in Layer 3, Layer 1 remains verifiable.

Concept:

// tests/runner.js
const targetLayer = process.argv[2] || 'latest';

// 1. Dynamically import the Factory for the requested layer
const layerPath = `../src/layer-${targetLayer}-*/index.js`; // (Glob pattern resolution logic needed here)
const factoryModule = await import(resolvePath(layerPath));
const factory = factoryModule[`createLayer${targetLayer}`];

// 2. Instantiate the System Under Test
const { interpreter } = factory();

// 3. Run the tests for this layer
const testSuite = await import(`./layer-${targetLayer}/tests.js`);
await testSuite.run(interpreter);


Usage

node tests/runner.js 1 -> Builds L1 Kernel, runs L1 tests.

node tests/runner.js 3 -> Builds L3 (which builds L2, which builds L1), then runs L3 tests.

7. Implementation Rules

To maintain sanity while building this:

The "Additive" Rule: If you need a new feature (e.g., vector-set!), add it to the primitives.js of the current layer you are building. Do not modify Layer 1 unless it is a fundamental bug fix.

The "Shadowing" Rule: If Layer 3 implements Cons cells, it should replace the list and map primitives from Layer 1 (which used Arrays) with new versions that handle Cons. This is done in createLayer3 by overwriting the environment bindings.

Transparent Interop: Keep using the direct mapping strategy (JS Numbers = Scheme Numbers). Only wrap types that absolutely require it (e.g., Cons, Symbol).

8. Immediate Next Steps

Refactor: Move current src/* files into src/layer-1-kernel/.

Create Factory: Create src/layer-1-kernel/index.js.

Implement Library Support: Add define-library and import handling to src/layer-1-kernel/analyzer.js (or a new library_manager.js).

Verify: Ensure node tests/runner.js 1 passes all existing tests.