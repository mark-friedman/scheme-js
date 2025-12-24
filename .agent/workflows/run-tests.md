---
description: how to run all tests
---

# Run All Tests

This workflow runs the complete test suite for the Scheme interpreter in both Node.js and browser environments.

## Node.js Tests

// turbo
1. Run the Node.js test suite:
   ```
   node run_tests_node.js
   ```
   This will execute all tests and display results in the terminal.

## Browser Tests

2. Ensure the HTTP server is running on port 8080:
   ```
   python3 -m http.server 8080
   ```
   (If already running, you can skip this step)

3. Open the browser REPL and test UI:
   ```
   open http://localhost:8080/web/ui.html
   ```
   This opens the interactive REPL in your default browser.

4. Check the browser developer console (F12) to see test results.

## Browser Testing Notes

- The browser UI provides an interactive REPL
- Tests run automatically on page load
- Test results are displayed in the browser console
- Verify that all tests pass in both Node.js and browser environments