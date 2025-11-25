# Global Project Rules

## Testing Requirements
- All new features must have accompanying unit and/or functional tests in `tests/`.
- Any complex logic should have unit tests.  Refactor to make it testable if needed.
- Ensure tests pass in both Node.js and the browser.

## Testing
- **Dual Environment**: All tests must run in both Node.js and the browser.
  - Use `if (typeof process !== 'undefined')` to guard Node.js-specific code (like `process.exitCode` or `import.meta.url` checks).
- **Structure**: Place new tests in `tests/` and register them in `tests/tests.js`.
- **Execution**:
  - Run `node run_tests_node.js` to verify changes locally.
  - Verify browser compatibility via `http://localhost:8080/ui.html`.

## Code Style
- **Modules**: Use ES Modules (`import`/`export`).
- **Formatting**: Use 2 spaces for indentation.
- **Exports**: Export functions and classes clearly.

## Documentation
- **Plans**: Update `layer_plan.md` when implementing new layers or significant features.
- **JSDoc**: Document all JavaScript functions with JSDoc.
- **Scheme Doc**: Document all Scheme functions with JSDoc-style comments, using the same format as JSDoc, but with Scheme procedure-level comment syntax (i.e. `;;`).
- **Internal Documentation**: Document logic inside JavaScript and Scheme functions and procedures using comment syntax appropriate for the language.
- **Code Sections**: Document the start of associated collections of functions and procedures using comment syntax appropriate for the language.
- **Directory Structure**: Generally, follow the directory structure outlined in `directory_structure.md`.  If you need to deviate from this structure or add to it, update `directory_structure.md` to reflect the change.
- **Walkthroughs**: Document the changes you make by appending them to `walkthrough.md`.


