---
trigger: always_on
---

# Global Project Rules

## Testing
- **Requirements**:
  - All new features must have accompanying unit and/or functional tests in `tests/`.
  - Any complex logic should have unit tests. Refactor to make it testable if needed.
  - Most tests should be written BEFORE the code that is being tested is written (TDD).
- **Dual Environment**: All tests must run in both Node.js and the browser.
  - Use `if (typeof process !== 'undefined')` to guard Node.js-specific code (like `process.exitCode` or `import.meta.url` checks).
- **Structure**: Place new tests in `tests/` and register them in `tests/test_manifest.js`.
- **Execution**:
  - Run `node run_tests_node.js` to verify changes locally.
  - Verify browser compatibility via `http://localhost:8080/ui.html`.

## Code Style
- **Modules**: Use ES Modules (`import`/`export`).
- **Formatting**: Use 2 spaces for indentation.
- **Exports**: Export functions and classes clearly.

## Code Organization
- **Directory and File Structure**: Follow the directory and file structure outlined in `directory_structure.md`. If you need to deviate from this structure or add to it, update `directory_structure.md` to reflect the change.

## Project Management
- **Task Tracking**: Maintain `task.md` as the source of truth for current progress.
- **Planning**: Create or update `implementation_plan.md` before starting major features or refactors.

## Documentation
- **JSDoc**: Document all JavaScript functions with JSDoc.
- **Scheme Doc**: Document all Scheme functions with JSDoc-style comments, using the same format as JSDoc, but with Scheme procedure-level comment syntax (i.e. `;;`).
- **Internal Documentation**: Document logic inside JavaScript and Scheme functions and procedures using comment syntax appropriate for the language.
- **Code Sections**: Document the start of associated collections of functions and procedures using comment syntax appropriate for the language (e.g. `;;; === SECTION NAME ===`).
- **CHANGES.md**: Document the changes you make by appending your walkthrough.md files to `CHANGES.md` when any major tasks are completed.

## Tools
### Scheme Conformance
- **Execution Based Conformance**: You can use the website at https://try.scheme.org/ to execute any Scheme code on a standard implementation.  This is not a substitute for testing, but it can be helpful for debugging or if you're unsure about the behavior of a Scheme language feature.
- **Standard Documentation Based Conformance**: The R7RS-small standard is available in `docs/r7rs-small.pdf`.  It is the primary reference for Scheme language features and behavior.
