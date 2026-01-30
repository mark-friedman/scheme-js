# Scheme Debugger Requirements

This document outlines the requirements and constraints for the `scheme-js` debugger, as specified by the user during the investigation and planning phase.

## Core Capabilities
- **Breakpoint Setting**: Support for setting breakpoints on Scheme source code
- **Step-by-Step Execution**: Support for basic stepping maneuvers (Step Into, Step Over, Step Out, Resume).
- **Source Location Tracking**: Ability to track and display source locations (line, column) for all evaluated Scheme code.
- **Multiple File Support**: Ability to view, browse, and set breakpoints across multiple JS and Scheme files (`.scm`, `.sld`) and code contexts.
- **Exception Handling**:
    - Support for "Break on Exception".
    - Option to break on both caught and uncaught errors/exceptions.
- **State Inspection**: A way to inspect lexical environments and global variables during execution.
- **Stack Traces**: Display of call stacks, with special consideration for maintaining logical traces in the presence of Tail Call Optimization (TCO).

## Polyglot Interoperability**:
- **JS-to-Scheme Step-In**: Seamless transition from the browser's native debugger into Scheme source code.
- **Scheme-to-JS Step-In**: Ability to hand off execution to the native debugger when entering JavaScript.
- **Boundary Synchronization**: Handling of "Step Over" and "Step Into" across the language boundary, including implicit calls (e.g., native functions stored in variables or `globalThis`).

## Modularity
- **Node.js Support**: The architecture must be modular enough to support future debugging of Node.js-based `scheme-js` applications (e.g., via CLI or DAP).

## Performance & Optimization
- **Toggleability**: Debugging overhead must be minimal when the feature is disabled.
- **Execution Model**: Use an asynchronous execution model (e.g., async trampoline) only when debugging to maintain UI responsiveness without freezing the tab.
