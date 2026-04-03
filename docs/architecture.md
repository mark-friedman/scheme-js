# Scheme Interpreter Architecture

R7RS-Small Scheme in JavaScript: minimal JS runtime, maximal Scheme libraries.

## Two-Tier Model

```
┌─────────────────────────────────────────────────────┐
│                   User Code                          │
├─────────────────────────────────────────────────────┤
│              R7RS Libraries (Scheme)                 │
│   (scheme base) (scheme write) (scheme read) ...    │
├─────────────────────────────────────────────────────┤
│              JavaScript Runtime                      │
│   Interpreter • Primitives • Library Loader          │
└─────────────────────────────────────────────────────┘
```

## JavaScript Runtime Components

| Component | Purpose |
|-----------|---------|
| `interpreter.js` | Trampoline execution loop |
| `stepables_base.js` | Register constants + `Executable` base class |
| `ast_nodes.js` | AST node classes (Literal, If, Lambda...) |
| `frames.js` | Continuation frame classes |
| `reader.js` | S-expression parser |
| `analyzer.js` | Dispatcher for S-exp → AST conversion |
| `analyzers/` | Modular handlers for special forms |
| `library_registry.js` | Feature + library registries |
| `library_parser.js` | define-library parser |
| `library_loader.js` | Library loading orchestration |
| `syntax_rules.js` | Macro transformer + hygiene primitives |
| `primitives/` | Native procedures |
| `primitives/io/` | Port system, Reader execution, Printer |

## Features Implemented in JS Core

| Feature | Description |
|---------|-------------|
| Trampoline | TCO via register machine |
| `call/cc` | First-class continuations |
| `dynamic-wind` | Before/after thunk protocol |
| Multiple Values | `values` / `call-with-values` |
| Hygienic Macros | Mark/rename algorithm |
| Exceptions | Handler stack, `raise`, `guard` |
| Parameters | `make-parameter`, `parameterize` |
| Library Loader | R7RS module system |
| Debugger | Breakpoints (line + expression-level), Stepping, Inspection, Cooperative Yielding |


## Directory Structure

```text
/
├── repl.js                         # Node.js REPL entry point
├── rollup.config.js                # Rollup bundling configuration
├── .github/                        # CI/CD Workflows
│   └── workflows/
│       └── ci.yml                  # GitHub Actions CI (Tests + Benchmarks)
├── benchmarks/                     # Performance Benchmarks
│   ├── run_benchmarks.js           # Core benchmark runner
│   ├── save_baseline.js            # Create/update baseline
│   ├── compare_baseline.js         # Compare current vs baseline
│   ├── baseline.json               # Recorded baseline metrics
│   └── *.scm                       # Benchmark definitions (arithmetic, mixed...)
├── src/
│   ├── packaging/                  # Bundling and distribution logic
│   │   ├── scheme_entry.js         # Core bundle entry point
│   │   ├── scheme_repl_wc.js       # Web Component entry point
│   │   └── html_adapter.js         # HTML script tag adapter
│   │
│   └── core/                       # The Core (JS Interpreter + Scheme subset)
│       ├── interpreter/            # JavaScript Interpreter
│       │   ├── index.js            # EXPORT: createInterpreter()
│       │   ├── interpreter.js      # Trampoline execution loop
│       │   ├── stepables.js        # Barrel file (re-exports all stepables)
│       │   ├── stepables_base.js   # Base class + register constants
│       │   ├── ast_nodes.js        # AST node classes (Literal, If, Lambda...)
│       │   ├── frames.js           # Continuation frame classes
│       │   ├── ast.js              # Legacy barrel file
│       │   ├── frame_registry.js   # Frame factory functions
│       │   ├── winders.js          # Dynamic-wind utilities
│       │   ├── environment.js      # Environment class
│       │   ├── errors.js           # SchemeError class
│       │   ├── values.js           # Closure, Continuation, TailCall, Values
│       │   ├── cons.js             # Cons cells + list utilities
│       │   ├── symbol.js           # Symbol interning
│       │   ├── reader.js           # S-expression parser (re-exports from reader/)
│       │   ├── reader/             # Reader submodules
│       │   │   ├── index.js        # Barrel export + parse() entry
│       │   │   ├── tokenizer.js    # Tokenization
│       │   │   ├── parser.js       # Core parsing logic
│       │   │   ├── number_parser.js # Number parsing with R7RS prefixes
│       │   │   ├── dot_access.js   # JS property access syntax
│       │   │   ├── string_utils.js # String/symbol escape processing
│       │   │   ├── character.js    # Character literal parsing
│       │   │   └── datum_labels.js # Circular reference handling
│       │   ├── analyzer.js         # S-exp → AST dispatcher
│       │   ├── analyzers/          # Modular special form handlers
│       │   │   ├── index.js        # Registry initialization
│       │   │   ├── registry.js     # Central handler registry
│       │   │   ├── core_forms.js   # quote, lambda, if, define
│       │   │   ├── control_forms.js # with-exception-handler, raise
│       │   │   └── module_forms.js  # import, define-library, cond-expand
│       │   ├── syntax_rules.js     # syntax-rules transformer
│       │   ├── syntax_object.js    # SyntaxObject and ScopeBindingRegistry
│       │   ├── macro_registry.js   # Macro registry
│       │   ├── identifier_utils.js # Shared identifier helpers
│       │   ├── type_check.js       # Type checking utilities for primitives
│       │   ├── library_loader.js   # Library loading + barrel (re-exports)
│       │   ├── library_registry.js # Feature + library registries
│       │   └── library_parser.js   # define-library parser
│       ├── primitives/             # Native procedures (+, cons, etc.)
│       │   ├── index.js            # Creates global environment
│       │   ├── math.js             # Arithmetic and numeric operations
│       │   ├── list.js             # List operations (cons, car, cdr, etc.)
│       │   ├── string.js           # String operations
│       │   ├── vector.js           # Vector operations
│       │   ├── control.js          # apply, map, call/cc
│       │   ├── char.js             # Character predicates and operations
│       │   ├── complex.js          # Complex number support
│       │   ├── rational.js         # Rational number support
│       │   ├── process_context.js  # exit, command-line, etc.
│       │   ├── time.js             # current-second, current-jiffy
│       │   ├── bytevector.js       # Bytevector operations (R7RS §6.9)
│       │   ├── class.js            # define-class support
│       │   ├── io/                 # Port system and I/O primitives
│       │   │   ├── index.js        # Barrel export
│       │   │   ├── ports.js        # Port base classes
│       │   │   ├── primitives.js   # Scheme binding definitions
│       │   │   ├── file_port.js    # File ports
│       │   │   ├── string_port.js  # String ports
│       │   │   ├── console_port.js # Console ports
│       │   │   ├── bytevector_port.js # Bytevector ports
│       │   │   ├── printer.js      # write/display logic
│       │   │   └── reader_bridge.js # read logic
│       │   ├── eq.js               # Equality predicates (eq?, eqv?, boolean=?)
│       │   ├── record.js           # define-record-type support
│       │   ├── exception.js        # Exception handling primitives
│       │   ├── interop.js          # JavaScript interop utilities
│       │   ├── async.js            # Async primitives (delay-resolve, etc.)
│       │   └── gc.js               # GC-related utilities
│       │
│       └── scheme/                 # Core Scheme subset (base library)
│           ├── base.sld            # (scheme base) library declaration
│           ├── core.sld            # (scheme core) library declaration
│           ├── control.sld         # (scheme control) library declaration
│           ├── cxr.sld             # (scheme cxr) library declaration
│           ├── char.sld            # (scheme char) library declaration
│           ├── write.sld           # (scheme write) library declaration
│           ├── read.sld            # (scheme read) library declaration
│           ├── file.sld            # (scheme file) library declaration
│           ├── repl.sld            # (scheme repl) library declaration
│           ├── complex.sld         # (scheme complex) library declaration
│           ├── eval.sld            # (scheme eval) library declaration
│           ├── lazy.sld            # (scheme lazy) library declaration
│           ├── process-context.sld # (scheme process-context)
│           ├── time.sld            # (scheme time) library declaration
│           ├── macros.scm          # Core macros: and, let, letrec, cond
│           ├── equality.scm        # Deep equality: equal?
│           ├── cxr.scm             # All 28 cxr accessors
│           ├── numbers.scm         # Variadic comparisons, predicates, min/max
│           ├── list.scm            # map, for-each, memq, assq, length, etc.
│           ├── control.scm         # when, unless, or, let*, do, case, guard
│           ├── parameter.scm       # make-parameter, parameterize
│           └── repl.scm            # REPL utilities
│
│   └── debug/                  # Debugger Runtime & Tools
│      ├── index.js            # Barrel export
│      ├── debug_level.js      # DebugLevel + DebugLevelStack (nested debugger contexts)
│      ├── scheme_debug_runtime.js # Central debugger coordinator
│      ├── debug_backend.js    # Abstract backend interface
│      ├── breakpoint_manager.js # Breakpoint registry (O(1) lookup, conditional breakpoints)
│      ├── stack_tracer.js     # Logical stack tracking (original names, TCO-aware)
│      ├── pause_controller.js # Stepping + cooperative polling (adaptive yield intervals)
│      ├── state_inspector.js  # Scope & value inspection (nameMap-aware)
│      ├── exception_handler.js # Error interception
│      ├── repl_debug_backend.js # REPL-specific backend adapter
│      ├── repl_debug_commands.js # REPL command parser (:break, :step, etc.)
│      └── devtools/            # Chrome DevTools Integration
│          ├── index.js         # Barrel export
│          ├── sourcemap_generator.js # V3 source map generation + VLQ encoder
│          ├── probe_generator.js # Probe script generation (one JS fn per Scheme expression)
│          ├── probe_runtime.js # __schemeProbeRuntime global (hit, stepping, breakpoints)
│          ├── source_registry.js # Source & probe management + expression spans + script injection + REPL LRU
│          ├── devtools_debug.js # DevToolsDebugIntegration (trampoline→probe bridge, REPL/library registration)
│          ├── scheme_debug_api.js # __schemeDebug global API installation: getStack/getLocals/eval/
│          │                    #   setBreakpoint/stepInto etc. + mapStackFrames helper + event wiring
│          ├── env_proxy.js     # Environment proxy for DevTools Scope pane
│          ├── sidebar_helpers.js # Sidebar data formatting utilities
│          └── custom_formatters.js # Chrome custom formatters for Scheme values
│
│   └── extras/                     # Extension libraries (non-R7RS)
│       ├── primitives/             # JavaScript primitives for extensions
│       │   ├── interop.js          # JS interop: js-eval, js-ref, js-set!
│       │   └── promise.js          # Promise interop primitives
│       └── scheme/                 # Scheme library files
│           ├── promise.sld         # (scheme-js promise) library declaration
│           └── promise.scm         # Promise utilities and macros
│
├── extension/                      # Chrome Extension — Standalone Debugger Window
│   ├── manifest.json               # Manifest V3 (debugger, scripting, tabs, action permissions)
│   ├── background.js               # Background service worker: CDP event routing, window lifecycle
│   │                               # (debugger windows map, toolbar icon click, sync-path pauses)
│   ├── activate_debug.js           # MAIN world content script at document_start: sets
│   │                               # __SCHEME_JS_DEBUG + loads breakpoints + panelConnected from localStorage
│   ├── content_script.js           # Isolated world content script: relays CustomEvents
│   │                               # (scheme-debug-paused / scheme-debug-resumed) → panel via sendMessage
│   ├── icons/                      # Extension icons (48px, 128px)
│   ├── build/
│   │   └── build-panel.js          # esbuild script: bundles panel-src + CodeMirror 6
│   ├── panel/                      # Built panel assets (generated — do not edit directly)
│   │   ├── panel.html              # Layout: toolbar, sidebar, editor, call stack, variables, console
│   │   ├── panel.css               # Adaptive dark/light theme (Dracula / GitHub-light)
│   │   └── panel.js                # Bundled JS output (generated by build-panel.js)
│   └── panel-src/                  # Panel source (ES modules, built by build-panel.js)
│       ├── main.js                 # Entry point — init + wire components; tabId from URL param,
│       │                           # navigation via tabs.onUpdated, theme via prefers-color-scheme
│       ├── breakpoint-state.js     # Breakpoint state management (JSON-keyed Map)
│       ├── splitter.js             # Generic drag-to-resize splitter
│       ├── language/
│       │   └── scheme-mode.js      # CodeMirror 6 Scheme language mode (lezer)
│       ├── components/
│       │   ├── editor.js           # CodeMirror 6 viewer (breakpoint gutter, line highlight,
│       │   │                       #   expression highlight, diamond markers, expression BPs)
│       │   ├── toolbar.js          # Debug controls: Resume, Step Into/Over/Out + status
│       │   ├── call-stack.js       # Call stack panel (Scheme + JS frames, TCO-aware)
│       │   ├── variables.js        # Variables panel (Local/Closure/Global scope grouping)
│       │   ├── breakpoints.js      # Breakpoints list panel
│       │   ├── console.js          # REPL eval console + page console output
│       │   └── source-list.js      # Source file browser (fetches from __schemeDebug)
│       └── protocol/
│           ├── constants.js        # Shared MSG and PAUSE_CONTEXT string enums (prevents typos)
│           ├── scheme-bridge.js    # Dual-path eval: chrome.scripting.executeScript (standalone)
│           │                       #   or inspectedWindow.eval (DevTools fallback)
│           ├── cdp-bridge.js       # CDP bridge: attach, step, resume, eval-while-paused,
│           │                       #   JS breakpoints, boundary breakpoints, source fetching
│           └── unified-debugger.js # Routes commands to correct bridge based on pause context
│                                   #   (scheme / js / scheme-sync), context-aware getters
│
├── tests/
│   ├── harness/                    # Test infrastructure
│   │   ├── helpers.js              # Test utilities (run, assert, createTestLogger)
│   │   ├── runner.js               # Test runner logic
│   │   └── scheme_test.scm         # Scheme test harness
│   │
│   ├── test_manifest.js            # Central registry of all test files
│   ├── run_all.js                  # Node.js test runner entry (Unit + Functional)
│   ├── run_scheme_tests.js         # Node.js Scheme test runner CLI
│   ├── run_scheme_tests_lib.js     # Shared Scheme test runner logic
│   ├── test_bundle.js              # Integration tests for bundled artifact
│   ├── test_script.scm             # Scheme script test for HTML adapter
│   │
│   ├── core/                       # Tests for src/core/
│   │   ├── interpreter/            # Tests for interpreter modules
│   │   │   ├── unit_tests.js
│   │   │   ├── reader/             # Reader submodule tests
│   │   │   ├── reader_tests.js
│   │   │   ├── nodes_tests.js      # AST node behavior tests
│   │   │   ├── frames_tests.js     # Continuation frame tests
│   │   │   ├── primitives_tests.js
│   │   │   ├── winders_tests.js
│   │   │   ├── syntax_rules_tests.js
│   │   │   ├── syntax_object_tests.js # Hygiene and scope tests
│   │   │   ├── data_tests.js
│   │   │   ├── error_tests.js
│   │   │   ├── interpreter_tests.js # Top-level interpreter logic
│   │   │   └── state_isolation_tests.js # Multi-context isolation tests
│   │   │
│   │   ├── primitives/             # Tests for primitives
│   │   │   └── io/                 # I/O unit tests
│   │   │       ├── string_port_tests.js
│   │   │       ├── file_port_tests.js
│   │   │       ├── bytevector_port_tests.js
│   │   │       └── printer_tests.js
│   │   │
│   │   └── scheme/                 # Scheme-based tests
│   │       ├── test.scm            # Scheme test harness
│   │       ├── primitive_tests.scm # Core primitives
│   │       ├── boot_tests.scm      # Environment bootstrap
│   │       ├── tco_tests.scm       # Tail call optimization
│   │       ├── dynamic_wind_tests.scm
│   │       ├── exception_tests.scm
│   │       ├── hygiene_tests.scm    # Basic hygiene
│   │       ├── macro_hygiene_tests.scm # Advanced hygiene suite
│   │       ├── parameter_tests.scm
│   │       ├── number_tests.scm     # Numeric tower (r7rs)
│   │       ├── list_tests.scm       # List library (r7rs)
│   │       ├── record_tests.scm     # Record types
│   │       ├── eval_tests.scm       # eval and environment
│   │       ├── repl_tests.scm
│   │       ├── cond_expand_tests.scm # cond-expand expression tests
│   │       └── compliance/         # R7RS conformance tests
│   │           ├── chibi_ui.html           # Browser UI for Chibi suite
│   │           ├── chibi_runner_lib.js     # Chibi test runner library
│   │           ├── run_chibi_tests.js      # Node.js runner for Chibi
│   │           ├── chapter_ui.html         # Browser UI for chapter tests
│   │           ├── chapter_runner_lib.js   # Chapter test runner library
│   │           ├── run_chapter_tests.js    # Node.js runner for chapters
│   │           ├── chapter_3.scm           # Basic concepts tests
│   │           ├── chapter_4.scm           # Expressions tests
│   │           ├── chapter_5.scm           # Program structure tests
│   │           ├── chapter_6.scm           # Standard procedures tests
│   │           └── chibi_revised/          # Chibi-based section tests
│   │               └── sections/           # Individual section files
│   │
│   ├── debug/                      # Debugger unit tests
│   │   ├── cooperative_pause_tests.js
│   │   ├── breakpoint_manager_tests.js
│   │   ├── stack_tracer_tests.js
│   │   ├── pause_controller_tests.js
│   │   ├── state_inspector_tests.js
│   │   └── devtools/               # DevTools integration unit tests
│   │       ├── scheme_debug_api_tests.js
│   │       ├── probe_generator_tests.js
│   │       ├── sourcemap_generator_tests.js
│   │       └── unified_debugger_tests.js
│   │
│   ├── extension/                  # Puppeteer E2E tests for the Chrome extension
│   │   ├── run_extension_tests.mjs # Test runner (--only filter supported)
│   │   ├── test_harness.mjs        # launchTestBrowser, waitFor, waitForPage helpers
│   │   ├── test_mock_chrome.mjs    # Shared mock chrome API builders
│   │   ├── test_activation.mjs     # Activation + source detection tests
│   │   ├── test_panel_interactions.mjs # Pause, step, frame click, variable display
│   │   ├── test_js_debugging.mjs   # CDP JS breakpoint + boundary stepping
│   │   ├── test_stepping.mjs       # Step into/over/out behavior
│   │   ├── test_inspection.mjs     # Variables + call stack inspection
│   │   ├── test_auto_resume.mjs    # Cooperative pause auto-resume behavior
│   │   ├── test_cdp_debugger_conflict.mjs # Probe hit / panelConnected interaction
│   │   ├── test_panel_e2e_breakpoint.mjs  # Real breakpoint → panel UI integration
│   │   ├── test_standalone_window.mjs     # Standalone window mock tests
│   │   ├── test_standalone_window_e2e.mjs # Standalone window E2E tests
│   │   ├── test_sync_callback_e2e.mjs     # Sync DOM callback breakpoint tests
│   │   ├── test_console.mjs        # Console eval + page output tests
│   │   ├── test_error_paths.mjs    # Error and failure path tests
│   │   ├── test_panel_ui.mjs       # Theme, layout, keyboard shortcuts
│   │   ├── test_relay.mjs          # Cross-world postMessage relay tests
│   │   └── test_js_interop.mjs     # JS/Scheme interop debugging
│   │
│   ├── functional/                 # Cross-cutting integration tests
│   │   ├── core_tests.js
│   │   ├── interop_tests.js
│   │   ├── macro_tests.js
│   │   ├── hygiene_tests.js
│   │   ├── io_tests.js
│   │   ├── string_tests.js
│   │   ├── vector_tests.js
│   │   ├── char_tests.js
│   │   └── ...
│   │
│   └── integration/                # Library system tests
│       ├── library_loader_tests.js
│       └── cond_expand_library_tests.js # cond-expand in libraries
│
├── scripts/
│   ├── generate_bundled_libraries.js # Embeds Scheme sources into JS bundle
│   └── package_extension.js       # Packages Chrome extension for distribution
│
├── docs/
│   ├── architecture.md             # High-level architecture (this file)
│   ├── core-interpreter-implementation.md  # Execution model details
│   ├── Interoperability.md         # JS/Scheme interop design
│   ├── hygiene.md                  # Macro hygiene algorithm (pure marks)
│   ├── hygiene_implementation.md   # Hygiene implementation internals
│   ├── macro_debugging.md          # Macro troubleshooting guide
│   ├── chrome_devtools_debugger_design.md  # DevTools debugger architecture & plan
│   ├── chrome_extension_manual.md  # User manual for the Chrome extension debugger
│   ├── debugging-flows.md          # Cross-cutting debugger flow documentation
│   ├── debugger_requirements.md    # Debugger requirements and design decisions
│   ├── REFERENCES.md               # Academic references
│   └── archive/                    # Archived planning documents
│
└── web/
    ├── ui.html                     # Browser REPL + test runner
    ├── main.js                     # Browser entry point
    └── repl.js                     # REPL UI logic
```

### Key Principles

1. **Two-Tier Model**: JavaScript provides the core; Scheme provides libraries.
2. **`src/core/`**: Everything needed to run basic Scheme (JS interpreter + core Scheme subset).
3. **`src/lib/`**: (Future) Additional R7RS libraries built on-top of the core.
4. **Tests mirror source**: `tests/core/` tests `src/core/`.
5. **Split Stepables**: AST nodes in `ast_nodes.js`, frames in `frames.js`, shared base in `stepables_base.js`.
6. **Split Library Loader**: Registry in `library_registry.js`, parser in `library_parser.js`, loader logic in `library_loader.js`.
7. **Modular Analyzer**: `analyzer.js` acts as a dispatcher to themed handlers in `analyzers/`, ensuring the analysis phase is extensible and isolated.
8. **Minimal Bootstrap**: Scheme libraries define what's needed to load `(scheme base)`.

## Related Documentation

- [core-interpreter-implementation.md](core-interpreter-implementation.md) — Execution model details
- [hygiene.md](hygiene.md) — Macro hygiene algorithm (pure marks)
- [hygiene_implementation.md](hygiene_implementation.md) — Hygiene implementation internals
- [macro_debugging.md](macro_debugging.md) — Troubleshooting common macro issues
- [chrome_extension_manual.md](chrome_extension_manual.md) — Chrome extension debugger user manual
- [debugging-flows.md](debugging-flows.md) — Cross-cutting debugger flow documentation
- [ROADMAP.md](../ROADMAP.md) — Implementation progress
