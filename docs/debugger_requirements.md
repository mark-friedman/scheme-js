# Scheme Debugger Requirements

This document outlines the requirements and constraints for the `scheme-js` debugger, as specified by the user during the investigation and planning phase.

## Core Capabilities
- **Browser use**: The primary use for this is when Scheme is used within web apps in a browser
- **Script tag support**: The debugger needs to recognize, properly display, allow breakpoints and stepping of Scheme and JavaScript code that is used in `script` tags.  That code may be in a separate file or within `script` tags in an HTML file.
- **Chrome Debugger**: It should operate within Chrome Dev Tools
- **Breakpoint Setting**: Support for setting breakpoints on Scheme expressions in source code.  The Scheme source code may be in a separate file or within `script` tags in an HTML file.
- **Step-by-Step Execution**: Support for basic stepping maneuvers (Step Into Expression/Statement, Step Over Expression/Statement, Step Out of Expression/Statement, Resume).
- **Source Location Tracking**: Ability to track and display source locations (line, column) for all evaluated Scheme code.
- **Multiple File Support**: Ability to view, browse, and set breakpoints across multiple HTML, JS and Scheme files (`.scm`, `.sld`) and code contexts.
- **Exception Handling**:
    - Support for "Break on Exception".
    - Option to break on both caught and uncaught errors/exceptions.
- **State Inspection**: A way to inspect lexical environments and global variables during execution.
- **Stack Traces**: Display of call stacks, with special consideration for maintaining logical traces in the presence of Tail Call Optimization (TCO).
- **Evaluation**: Enable a REPL which would execute code in the context of the currently selected stack location
- **Professional Quality**: This debugger should be something that a professional web app builder and Scheme/Lisp
  programmer should find familiar, comfortable and useful.

## Polyglot Interoperability**:
- **JS-to-Scheme Step-In and Step-Out**: Seamless transition from JavaScript debugging into Scheme source code.
- **Scheme-to-JS Step-In and Step-Out**: Seamless transition from Scheme debugging into JavaScript.
- **Boundary Synchronization**: Handling of "Step Over" and "Step Into" across the language boundary, including implicit calls (e.g., native functions stored in variables or `globalThis`).

## Modularity
- **Node.js Support**: The architecture must be modular enough to support future debugging of Node.js-based `scheme-js` applications (e.g., via CLI or DAP).

## Performance & Optimization
- **Toggleability**: Debugging overhead must be minimal when the feature is disabled.
- **Reasonable**: Debugging overhead should be less than 20% when the Chrome dev tools panel is open
