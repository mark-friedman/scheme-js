---
description: how to run all tests
---

# Run All Tests

This workflow runs the complete test suite for the Scheme interpreter in both Node.js and browser environments.

## Prerequisites

1. Ensure the HTTP server is running on port 8080:
   ```
   python3 -m http.server 8080
   ```
   (If already running, you can skip this step)

## Run Tests

2. Run the Node.js test suite:
// turbo
   ```
   node run_tests_node.js
   ```
   This will execute all tests and display results in the terminal.

3. Open the browser test UI:
   ```
   open http://localhost:8080/tests.html
   ```
   This opens the interactive test runner in your default browser.

4. Verify that all tests pass in both environments.

## Browser Testing Notes

- The browser UI provides an interactive REPL in addition to running tests
- Test results are displayed directly in the browser interface
- Check the browser console for any additional debug output