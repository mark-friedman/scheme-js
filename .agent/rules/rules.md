---
trigger: always_on
---

# Global Project Rules

## Agentic Rules
- **Following Orders**: DO NOT make any changes or carry our implementation tasks if the user just asks a question.  Just answer the question!

## Testing Requirements
- All new features must have accompanying unit and/or functional tests in `tests/`.
- Any complex logic should have unit tests.  Refactor to make it testable if needed.
- Ensure tests pass in both Node.js and the browser.
- Most tests should be written BEFORE the code that is being tested is written. Sometimes you'll realize after writing the code that you need to write additional tests for it and that's ok.  It's also ok to occasionally rewrite tests to make them more correct or more effective or cover more cases.

## Testing
- **Dual Environment**: All tests must run in both Node.js and the browser.
  - Use `if (typeof process !== 'undefined')` to guard Node.js-specific code (like `process.exitCode` or `import.meta.url` checks).
- **Structure**: Place new tests in `tests/` and register them in `tests/tests.js`.
- **Execution**:
  - Run `node run_tests_node.js` to verify changes locally.
  - Verify browser compatibility via `http://localhost:8080/ui.html`.

## JavaScript Code Style
- **Modules**: Use ES Modules (`import`/`export`).
- **Formatting**: Use 2 spaces for indentation.
- **Exports**: Export functions and classes clearly.

## Scheme Code Rules
- **Type Checking**: All standard Scheme procedures (i.e. the ones in the r7rs-small standard) must be implemented with all neccessary type, range, and arity checking.
- **Scheme over JS**: Implementations should always be done in Scheme, if possible.  If that's not possible, isolate the minimum that is required in JavaScript and then implement the rest in Scheme.

## Code Organization
- **Directory and File Structure**: Follow the directory and file structure outlined in `directory_structure.md`.  If you need to deviate from this structure or add to it, update `directory_structure.md` to reflect the change.
- **Scheme Dependencies**: Scheme procedures and special forms should be described in Scheme `.sld` files in libraries using `define-library` to express their exports, dependencies, etc. Generally the `.sld` files only describe the libraries.  The actual implementations should be in separate `.scm` files. For standard Scheme procedures (i.e. the ones in the r7rs-small standard) use appendix A of the r7rs-small standard to determine the libraries that those procedures should be defined in.


## Documentation
- **JSDoc**: Document all JavaScript functions with JSDoc.
- **Scheme Doc**: Document all Scheme functions with JSDoc-style comments, using the same format as JSDoc, but with Scheme procedure-level comment syntax (i.e. `;;`).
- **Internal Documentation**: Document logic inside JavaScript and Scheme functions and procedures using comment syntax appropriate for the language.
- **Code Sections**: Document the start of associated collections of functions and procedures using comment syntax appropriate for the language.
- **Directory Structure**: Generally, follow the directory structure outlined in `directory_structure.md`.  If you need to deviate from this structure or add to it, update `directory_structure.md` to reflect the change.
- **CHANGES.md**: Document the changes you make by appending your walkthrough.md files to `CHANGES.md` when any major talks are completed.
- **Roadmap**:  Use the `r7rs_roadmap.md` file to record the general progress and plan for the project.  Keep it updated with work that has been done and add to it to reflect work planned for the future.

## Tools
### Scheme Conformance
- **Execution Based Conformance**: You can use the website at https://try.scheme.org/ to execute any Scheme code on a standard implementation.  This is not a substitute for testing, but it can be helpful for debugging or if you're unsure about the behavior of a Scheme language feature.
- **Standard Documentation Based Conformance**: The R7RS-small standard is available in `docs/r7rs-small.pdf`.  It is the primary reference for Scheme language features and behavior.