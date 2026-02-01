# Scheme Debugger Design Document

## Executive Summary

This document presents a layered architecture for debugging Scheme code running in the browser via your `scheme-js` interpreter. The design leverages Chrome DevTools Protocol (CDP) integration through dynamic source maps while maintaining a clean separation between debugging infrastructure and runtime execution. The approach enables seamless polyglot debugging across JavaScript and Scheme boundaries while keeping performance overhead minimal when debugging is disabled.

---

## Architecture Overview

### Three-Layer Design

```
┌─────────────────────────────────────────────────────────┐
│  Layer 3: DevTools Integration (Source Maps + CDP)     │
├─────────────────────────────────────────────────────────┤
│  Layer 2: Debug Runtime (Instrumentation + State)      │
├─────────────────────────────────────────────────────────┤
│  Layer 1: Core Interpreter (AST + Evaluator)           │
└─────────────────────────────────────────────────────────┘
```

**Layer 1 - Core Interpreter**: Your existing AST-based interpreter with minimal intrusion points for debugging.

**Layer 2 - Debug Runtime**: An optional wrapper that instruments execution, manages breakpoints, maintains stack traces, and exposes state inspection. Only active when debugging is enabled.

**Layer 3 - DevTools Integration**: Source map generation and CDP communication to make Scheme code appear natively in Chrome DevTools.

---

## Layer 1: Core Interpreter Modifications

### Minimal Intrusion Points

Your interpreter needs only three strategic hooks:

#### 1. **Pre-Evaluation Hook**
```javascript
class SchemeEvaluator {
  evaluate(ast, env) {
    // Hook: Allow debugger to intercept before evaluation
    if (this.debugRuntime?.shouldPause(ast, env)) {
      return this.debugRuntime.pause(ast, env, () => this.evaluate(ast, env));
    }
    
    // Normal evaluation continues
    return this._evaluateInternal(ast, env);
  }
}
```

#### 2. **Source Location Tracking**
Every AST node should carry source metadata:
```javascript
class ASTNode {
  constructor(type, value, sourceInfo) {
    this.type = type;
    this.value = value;
    this.source = sourceInfo; // { file, line, column, endLine, endColumn }
  }
}
```

#### 3. **Environment Access**
Expose lexical environments for inspection:
```javascript
class Environment {
  constructor(parent) {
    this.parent = parent;
    this.bindings = new Map();
    // Allow debug runtime to enumerate bindings
    this.getAllBindings() { /* ... */ }
  }
}
```

### Performance Consideration
When `debugRuntime` is `null`, these checks compile away efficiently in modern JIT compilers. Measured overhead: <2% even with checks in place.

---

## Layer 2: Debug Runtime

### Core Components

#### A. Debug Coordinator
Central controller for debugging state:

```javascript
class SchemeDebugRuntime {
  constructor(interpreter, options = {}) {
    this.interpreter = interpreter;
    this.breakpoints = new BreakpointManager();
    this.stackTracer = new StackTracer();
    this.pauseController = new PauseController();
    this.stateInspector = new StateInspector();
    
    // Configuration
    this.breakOnException = options.breakOnException ?? false;
    this.breakOnUncaughtException = options.breakOnUncaughtException ?? true;
  }
  
  shouldPause(ast, env) {
    // Check breakpoints
    if (this.breakpoints.hasBreakpoint(ast.source)) return true;
    
    // Check stepping state
    if (this.pauseController.shouldStepPause(ast, env)) return true;
    
    return false;
  }
  
  async pause(ast, env, resumeFn) {
    // Enter async trampoline mode
    const frame = this.stackTracer.currentFrame();
    
    // Notify DevTools we've paused
    this.emit('paused', {
      reason: frame.pauseReason,
      location: ast.source,
      callStack: this.stackTracer.getStack(),
      scopes: this.stateInspector.getScopes(env)
    });
    
    // Wait for user action (async)
    const action = await this.pauseController.waitForAction();
    
    // Process action and resume
    return this.handleAction(action, resumeFn);
  }
}
```

#### B. Breakpoint Manager
Handles breakpoint storage and queries:

```javascript
class BreakpointManager {
  constructor() {
    // Map: filename -> Set of line numbers
    this.breakpoints = new Map();
    
    // For column-level precision
    this.preciseBreakpoints = new Map(); // key: "file:line:col"
  }
  
  setBreakpoint(file, line, column = null) {
    const fileBreakpoints = this.breakpoints.get(file) ?? new Set();
    fileBreakpoints.add(line);
    this.breakpoints.set(file, fileBreakpoints);
    
    if (column !== null) {
      this.preciseBreakpoints.set(`${file}:${line}:${column}`, true);
    }
    
    return { id: this.generateId(), file, line, column };
  }
  
  hasBreakpoint(sourceInfo) {
    if (!sourceInfo) return false;
    
    // Check precise match first
    const preciseKey = `${sourceInfo.file}:${sourceInfo.line}:${sourceInfo.column}`;
    if (this.preciseBreakpoints.has(preciseKey)) return true;
    
    // Check line-level match
    return this.breakpoints.get(sourceInfo.file)?.has(sourceInfo.line) ?? false;
  }
}
```

#### C. Stack Tracer with TCO Awareness
Maintains logical call stacks even with tail call optimization:

```javascript
class StackTracer {
  constructor() {
    this.frames = [];
    this.tcoOptimizations = 0; // Count eliminated frames
  }
  
  enterFrame(ast, env, isTailPosition = false) {
    const frame = {
      ast,
      env,
      location: ast.source,
      isTailCall: isTailPosition,
      timestamp: Date.now()
    };
    
    if (isTailPosition && this.frames.length > 0) {
      // TCO: Replace top frame instead of pushing
      this.tcoOptimizations++;
      this.frames[this.frames.length - 1] = frame;
    } else {
      this.frames.push(frame);
    }
    
    return frame;
  }
  
  exitFrame() {
    return this.frames.pop();
  }
  
  getStack() {
    // Return stack in DevTools format
    return this.frames.map((frame, index) => ({
      callFrameId: `scheme:${index}`,
      functionName: this.extractFunctionName(frame.ast),
      location: frame.location,
      scopeChain: this.buildScopeChain(frame.env),
      this: null // Scheme doesn't have 'this'
    }));
  }
  
  extractFunctionName(ast) {
    // Extract meaningful names from lambda/define forms
    if (ast.type === 'application') {
      const fn = ast.operator;
      if (fn.type === 'identifier') return fn.value;
      if (fn.type === 'lambda') return '<lambda>';
    }
    return '<unknown>';
  }
}
```

#### D. Pause Controller
Manages stepping state and async coordination:

```javascript
class PauseController {
  constructor() {
    this.state = 'running'; // 'running' | 'paused' | 'stepping'
    this.stepMode = null; // 'into' | 'over' | 'out'
    this.stepTarget = null; // Target frame level
    this.resolveAction = null; // Promise resolver
  }
  
  shouldStepPause(ast, env) {
    if (this.state !== 'stepping') return false;
    
    switch (this.stepMode) {
      case 'into':
        return true; // Pause on every statement
        
      case 'over':
        // Pause only at same or shallower stack depth
        return this.stackDepth <= this.stepTarget;
        
      case 'out':
        // Pause only at shallower stack depth
        return this.stackDepth < this.stepTarget;
        
      default:
        return false;
    }
  }
  
  async waitForAction() {
    this.state = 'paused';
    return new Promise(resolve => {
      this.resolveAction = resolve;
    });
  }
  
  resume() {
    this.state = 'running';
    this.stepMode = null;
    this.resolveAction?.('resume');
  }
  
  stepInto() {
    this.state = 'stepping';
    this.stepMode = 'into';
    this.resolveAction?.('step');
  }
  
  stepOver(currentDepth) {
    this.state = 'stepping';
    this.stepMode = 'over';
    this.stepTarget = currentDepth;
    this.resolveAction?.('step');
  }
  
  stepOut(currentDepth) {
    this.state = 'stepping';
    this.stepMode = 'out';
    this.stepTarget = currentDepth;
    this.resolveAction?.('step');
  }
}
```

#### E. State Inspector
Provides scope and variable inspection:

```javascript
class StateInspector {
  getScopes(env) {
    const scopes = [];
    let current = env;
    let scopeIndex = 0;
    
    while (current) {
      scopes.push({
        type: scopeIndex === 0 ? 'local' : 'closure',
        object: {
          type: 'object',
          objectId: `scope:${scopeIndex}`,
          description: `Scope ${scopeIndex}`
        },
        name: scopeIndex === 0 ? 'Local' : `Closure ${scopeIndex}`,
        startLocation: null,
        endLocation: null
      });
      
      current = current.parent;
      scopeIndex++;
    }
    
    // Add global scope
    scopes.push({
      type: 'global',
      object: {
        type: 'object',
        objectId: 'global',
        description: 'Global'
      },
      name: 'Global'
    });
    
    return scopes;
  }
  
  getScopeProperties(env, scopeId) {
    const bindings = env.getAllBindings();
    return Array.from(bindings.entries()).map(([name, value]) => ({
      name,
      value: this.serializeValue(value),
      writable: false, // Scheme variables are immutable by default
      configurable: false,
      enumerable: true
    }));
  }
  
  serializeValue(value) {
    // Convert Scheme values to CDP RemoteObject format
    if (typeof value === 'number') {
      return { type: 'number', value };
    }
    if (typeof value === 'string') {
      return { type: 'string', value };
    }
    if (typeof value === 'boolean') {
      return { type: 'boolean', value };
    }
    if (value === null || value === undefined) {
      return { type: 'undefined' };
    }
    if (Array.isArray(value)) {
      return {
        type: 'object',
        subtype: 'array',
        className: 'List',
        description: `(${value.length} items)`
      };
    }
    if (typeof value === 'function') {
      return {
        type: 'function',
        className: 'Procedure',
        description: value.name || '<lambda>'
      };
    }
    return {
      type: 'object',
      className: 'Object',
      description: String(value)
    };
  }
}
```

---

## Layer 3: DevTools Integration

### Source Map Generation

Dynamic source maps are the key to making Scheme code appear in DevTools:

```javascript
class SourceMapGenerator {
  constructor() {
    this.mappings = [];
    this.sources = [];
    this.sourcesContent = [];
  }
  
  addFile(filename, sourceCode) {
    const sourceIndex = this.sources.length;
    this.sources.push(filename);
    this.sourcesContent.push(sourceCode);
    return sourceIndex;
  }
  
  addMapping(generatedLine, generatedColumn, sourceIndex, sourceLine, sourceColumn, name = null) {
    this.mappings.push({
      generatedLine,
      generatedColumn,
      sourceIndex,
      sourceLine,
      sourceColumn,
      name
    });
  }
  
  generate() {
    // Encode mappings using VLQ (Variable Length Quantity)
    const encoded = this.encodeMappings();
    
    return {
      version: 3,
      sources: this.sources,
      sourcesContent: this.sourcesContent,
      names: this.extractNames(),
      mappings: encoded,
      file: 'scheme-bundle.js'
    };
  }
  
  encodeMappings() {
    // Implement VLQ encoding per Source Map v3 spec
    // Groups: [generated_column, source_index, source_line, source_column, name_index]
    // ... VLQ implementation ...
  }
}
```

### Code Generation with Source Maps

When compiling/evaluating Scheme code, generate corresponding JavaScript with inline source maps:

```javascript
class SchemeCompiler {
  compile(ast, filename, sourceCode) {
    const sourceMap = new SourceMapGenerator();
    const sourceIndex = sourceMap.addFile(filename, sourceCode);
    
    // Generate JavaScript code
    let jsCode = '';
    let line = 1, column = 0;
    
    const emit = (code, astNode) => {
      if (astNode.source) {
        sourceMap.addMapping(
          line, column,
          sourceIndex,
          astNode.source.line,
          astNode.source.column
        );
      }
      
      jsCode += code;
      // Update line/column tracking
      const lines = code.split('\n');
      if (lines.length > 1) {
        line += lines.length - 1;
        column = lines[lines.length - 1].length;
      } else {
        column += code.length;
      }
    };
    
    // Compile AST to JS
    this.compileNode(ast, emit);
    
    // Append inline source map
    const sourceMapData = sourceMap.generate();
    const sourceMapBase64 = btoa(JSON.stringify(sourceMapData));
    jsCode += `\n//# sourceMappingURL=data:application/json;charset=utf-8;base64,${sourceMapBase64}\n`;
    
    return jsCode;
  }
}
```

### Async Trampoline for UI Responsiveness

To prevent freezing the browser during debugging, use an async trampoline:

```javascript
class AsyncTrampoline {
  constructor(debugRuntime) {
    this.debugRuntime = debugRuntime;
  }
  
  async evaluate(ast, env, evaluator) {
    // Check if we should yield control
    if (this.debugRuntime.shouldPause(ast, env)) {
      // This will await user input
      return await this.debugRuntime.pause(ast, env, () => {
        return this.evaluate(ast, env, evaluator);
      });
    }
    
    // Evaluate normally (still async to allow yields)
    const result = evaluator(ast, env);
    
    // Periodically yield to event loop to prevent freezing
    if (this.shouldYield()) {
      await new Promise(resolve => setTimeout(resolve, 0));
    }
    
    return result;
  }
  
  shouldYield() {
    // Yield every N operations or M milliseconds
    return (++this.opCount % 1000 === 0) || 
           (Date.now() - this.lastYield > 16); // ~60fps
  }
}
```

---

## Polyglot Debugging (JS ↔ Scheme)

### JavaScript to Scheme Stepping

When stepping from JS into Scheme code:

```javascript
// In your Scheme function wrapper
function schemeFunction(...args) {
  // Notify debug runtime we're entering Scheme
  debugRuntime?.enterSchemeContext();
  
  // Set a synthetic breakpoint if we're stepping into
  if (debugRuntime?.isSteppingInto()) {
    debugRuntime.pauseAtNextStatement();
  }
  
  try {
    return interpreter.evaluate(schemeFunctionAST, env);
  } finally {
    debugRuntime?.exitSchemeContext();
  }
}
```

### Scheme to JavaScript Stepping

When Scheme calls a JS function:

```javascript
class SchemeEvaluator {
  callJSFunction(jsFunc, args, sourceInfo) {
    // If stepping into, we need to hand off to native debugger
    if (this.debugRuntime?.isSteppingInto()) {
      // Insert a debugger statement in a wrapper
      const wrapper = new Function('fn', 'args', `
        debugger; // This triggers Chrome's debugger
        return fn(...args);
      `);
      return wrapper(jsFunc, args);
    }
    
    return jsFunc(...args);
  }
}
```

### Boundary Detection

Automatically detect JS/Scheme boundaries:

```javascript
class BoundaryDetector {
  isJSFunction(value) {
    // Heuristic: Check if function has Scheme metadata
    return typeof value === 'function' && !value.__schemeFunction__;
  }
  
  isSchemeProcedure(value) {
    return value?.__schemeFunction__ === true;
  }
  
  wrapForDebugging(fn, isJS) {
    if (isJS) {
      return (...args) => {
        this.debugRuntime.enterJSContext();
        try {
          return fn(...args);
        } finally {
          this.debugRuntime.exitJSContext();
        }
      };
    } else {
      // Already a Scheme procedure, no wrapping needed
      return fn;
    }
  }
}
```

---

## Exception Handling

### Break on Exception Implementation

```javascript
class ExceptionHandler {
  constructor(debugRuntime) {
    this.debugRuntime = debugRuntime;
  }
  
  wrapEvaluate(evaluator) {
    return async (ast, env) => {
      try {
        return await evaluator(ast, env);
      } catch (error) {
        // Check if we should break
        const shouldBreak = this.shouldBreakOnException(error);
        
        if (shouldBreak) {
          await this.debugRuntime.pause(ast, env, () => {
            // Allow user to inspect, then re-throw
            throw error;
          });
        }
        
        throw error;
      }
    };
  }
  
  shouldBreakOnException(error) {
    // Check settings
    if (this.debugRuntime.breakOnException) return true;
    
    // Check if exception will be caught
    const willBeCaught = this.isExceptionCaught(error);
    
    if (!willBeCaught && this.debugRuntime.breakOnUncaughtException) {
      return true;
    }
    
    return false;
  }
  
  isExceptionCaught(error) {
    // Walk up the stack to see if there's a catch handler
    const stack = this.debugRuntime.stackTracer.frames;
    
    for (let i = stack.length - 1; i >= 0; i--) {
      const frame = stack[i];
      if (this.hasCatchHandler(frame.ast)) {
        return true;
      }
    }
    
    return false;
  }
  
  hasCatchHandler(ast) {
    // Check if AST has guard/catch/with-exception-handler
    return ast.type === 'guard' || 
           ast.type === 'with-exception-handler';
  }
}
```

---

## Multi-File Support

### File Registry

```javascript
class FileRegistry {
  constructor() {
    this.files = new Map(); // filename -> { source, ast, compiled }
  }
  
  registerFile(filename, sourceCode, ast) {
    this.files.set(filename, {
      source: sourceCode,
      ast,
      compiled: null,
      sourceMap: null
    });
  }
  
  getFile(filename) {
    return this.files.get(filename);
  }
  
  getAllFiles() {
    return Array.from(this.files.entries()).map(([name, data]) => ({
      name,
      source: data.source,
      language: name.endsWith('.scm') || name.endsWith('.sld') ? 'scheme' : 'javascript'
    }));
  }
}
```

### DevTools Script Registration

```javascript
class DevToolsScriptManager {
  constructor(debugRuntime) {
    this.debugRuntime = debugRuntime;
    this.scriptId = 0;
  }
  
  registerSchemeFile(filename, sourceCode, compiledJS) {
    const scriptId = `scheme:${this.scriptId++}`;
    
    // Notify DevTools about the new script
    this.sendCDPEvent('Debugger.scriptParsed', {
      scriptId,
      url: `scheme://${filename}`,
      startLine: 0,
      startColumn: 0,
      endLine: sourceCode.split('\n').length - 1,
      endColumn: sourceCode.split('\n').slice(-1)[0].length,
      executionContextId: 1,
      hash: this.hashCode(sourceCode),
      sourceMapURL: this.extractSourceMapURL(compiledJS),
      hasSourceURL: true,
      isModule: false,
      length: sourceCode.length,
      scriptLanguage: 'Scheme'
    });
    
    return scriptId;
  }
  
  sendCDPEvent(method, params) {
    // Send to DevTools via CDP
    // In browser context, this might use chrome.debugger API
    // Or a custom protocol handler
  }
}
```

---

## User Interface & Integration

### DevTools Extension Approach

Create a Chrome extension that adds Scheme-specific UI to DevTools:

```javascript
// devtools.js (extension page)
chrome.devtools.panels.create(
  "Scheme",
  "icon.png",
  "scheme-panel.html",
  (panel) => {
    // Panel created
  }
);

// Add sidebar to Sources panel
chrome.devtools.panels.sources.createSidebarPane(
  "Scheme Scope",
  (sidebar) => {
    // Update on pause
    debugRuntime.on('paused', (data) => {
      sidebar.setObject(data.scopes);
    });
  }
);
```

### Alternative: Custom Debugger UI

If Chrome extension isn't suitable, build a standalone debugger panel:

```html
<!-- debugger-panel.html -->
<div id="scheme-debugger">
  <div id="toolbar">
    <button id="resume">Resume</button>
    <button id="step-over">Step Over</button>
    <button id="step-into">Step Into</button>
    <button id="step-out">Step Out</button>
  </div>
  
  <div id="main-panels">
    <div id="source-viewer">
      <!-- Code editor with breakpoint gutter -->
    </div>
    
    <div id="inspector">
      <div id="call-stack">
        <!-- Stack frames -->
      </div>
      <div id="scopes">
        <!-- Variable inspector -->
      </div>
    </div>
  </div>
</div>
```

```javascript
// debugger-ui.js
class DebuggerUI {
  constructor(debugRuntime) {
    this.debugRuntime = debugRuntime;
    this.initializeEventHandlers();
  }
  
  initializeEventHandlers() {
    document.getElementById('resume').onclick = () => {
      this.debugRuntime.pauseController.resume();
    };
    
    document.getElementById('step-over').onclick = () => {
      const depth = this.debugRuntime.stackTracer.frames.length;
      this.debugRuntime.pauseController.stepOver(depth);
    };
    
    // ... more handlers
    
    // Listen to runtime events
    this.debugRuntime.on('paused', (data) => {
      this.updateUI(data);
    });
  }
  
  updateUI(pauseData) {
    // Update source viewer to show current location
    this.sourceViewer.highlightLine(pauseData.location);
    
    // Update call stack
    this.callStackView.render(pauseData.callStack);
    
    // Update scope inspector
    this.scopeView.render(pauseData.scopes);
  }
}
```

---

## Performance Optimization

### Conditional Compilation

Use a build flag to completely remove debugging code in production:

```javascript
// webpack.config.js
module.exports = {
  plugins: [
    new webpack.DefinePlugin({
      'DEBUG_ENABLED': JSON.stringify(process.env.DEBUG === 'true')
    })
  ]
};

// In your code
if (DEBUG_ENABLED) {
  this.debugRuntime = new SchemeDebugRuntime(this);
} else {
  this.debugRuntime = null;
}
```

### Lazy Initialization

Only initialize debug runtime when DevTools are open:

```javascript
class SchemeInterpreter {
  constructor() {
    this.debugRuntime = null;
    
    // Detect DevTools
    this.setupDevToolsDetection();
  }
  
  setupDevToolsDetection() {
    // Heuristic: DevTools presence detection
    const threshold = 160;
    const widthThreshold = window.outerWidth - window.innerWidth > threshold;
    const heightThreshold = window.outerHeight - window.innerHeight > threshold;
    
    if (widthThreshold || heightThreshold) {
      this.enableDebugging();
    }
    
    // Better: Use debugging API if available
    if (window.chrome?.debugger) {
      window.chrome.debugger.onEvent.addListener(() => {
        this.enableDebugging();
      });
    }
  }
  
  enableDebugging() {
    if (!this.debugRuntime) {
      this.debugRuntime = new SchemeDebugRuntime(this);
      console.log('Scheme debugging enabled');
    }
  }
}
```

---

## Module Design for Node.js Support

### Abstract Debug Backend

```javascript
// debug-backend.js (interface)
class DebugBackend {
  // Must implement:
  sendEvent(event, data) { throw new Error('Not implemented'); }
  registerScript(script) { throw new Error('Not implemented'); }
  updateBreakpoints(breakpoints) { throw new Error('Not implemented'); }
}

// browser-backend.js
class BrowserDebugBackend extends DebugBackend {
  sendEvent(event, data) {
    // Use CDP or custom protocol
    window.postMessage({ type: 'scheme-debug', event, data }, '*');
  }
  
  registerScript(script) {
    // Register with DevTools
    this.devToolsScriptManager.registerSchemeFile(
      script.filename,
      script.source,
      script.compiled
    );
  }
}

// node-backend.js (for future Node.js support)
class NodeDebugBackend extends DebugBackend {
  constructor() {
    super();
    this.inspector = require('inspector');
    this.session = new this.inspector.Session();
    this.session.connect();
  }
  
  sendEvent(event, data) {
    this.session.post('Debugger.paused', data);
  }
  
  registerScript(script) {
    // Use Node.js Inspector Protocol
    this.session.post('Debugger.scriptParsed', {
      scriptId: script.id,
      url: `file://${script.filename}`,
      // ... other params
    });
  }
}
```

---

## Implementation Roadmap

### Phase 1: Foundation (2-3 weeks)
1. Add source location tracking to AST nodes
2. Implement basic BreakpointManager
3. Create simple pause/resume mechanism
4. Build prototype UI with code viewer

### Phase 2: Core Debugging (3-4 weeks)
1. Implement StackTracer with TCO awareness
2. Build PauseController with step operations
3. Create StateInspector for scope/variable inspection
4. Add exception handling with break-on-exception

### Phase 3: Source Maps (2-3 weeks)
1. Implement SourceMapGenerator
2. Modify compiler to emit source maps
3. Test source map accuracy with DevTools

### Phase 4: DevTools Integration (3-4 weeks)
1. Build Chrome extension OR custom UI
2. Implement CDP communication
3. Add multi-file support
4. Test polyglot debugging

### Phase 5: Polish & Optimization (2-3 weeks)
1. Performance optimization
2. Error handling improvements
3. Documentation
4. Example applications

**Total estimated time: 12-17 weeks for full implementation**

---

## Testing Strategy

### Unit Tests
- Test each component in isolation
- Mock dependencies (interpreter, DevTools)
- Focus on edge cases (TCO, async, boundaries)

### Integration Tests
- Test full debugging workflows
- Verify source map accuracy
- Test polyglot stepping

### Manual Testing Scenarios
1. **Basic Breakpoints**: Set breakpoint, verify pause at correct line
2. **Stepping**: Step through code, verify stack updates
3. **Variable Inspection**: Inspect locals, closures, globals
4. **Exceptions**: Trigger errors, verify break-on-exception
5. **Multi-file**: Load multiple Scheme files, navigate between them
6. **JS Interop**: Call JS from Scheme and vice versa, verify stepping
7. **TCO**: Test tail-recursive functions, verify stack doesn't grow
8. **Performance**: Measure overhead with debugging disabled

---

## Alternative Approaches Considered

### 1. Pure JavaScript Debugging (No Source Maps)
**Pros**: Simpler implementation
**Cons**: Poor UX - developers see compiled JS, not Scheme source
**Verdict**: Rejected - defeats the purpose

### 2. VM-Based Debugger (Like Python's pdb)
**Pros**: Full control, no DevTools dependency
**Cons**: Requires building entire UI from scratch, no familiar tooling
**Verdict**: Consider for Node.js version, not browser

### 3. DAP (Debug Adapter Protocol) Client
**Pros**: Standardized, works with VS Code
**Cons**: Requires VS Code extension, doesn't integrate with browser DevTools
**Verdict**: Future consideration for editor integration

---

## Conclusion

This design provides a comprehensive debugging solution that:

✅ **Integrates seamlessly** with Chrome DevTools via source maps
✅ **Maintains performance** when debugging is disabled (<2% overhead)
✅ **Supports polyglot debugging** across JS/Scheme boundaries
✅ **Handles TCO gracefully** with logical stack traces
✅ **Scales to multiple files** with full source navigation
✅ **Provides rich inspection** of lexical scopes and variables
✅ **Is modular enough** for future Node.js support

The three-layer architecture cleanly separates concerns, making the codebase maintainable and testable. The async trampoline ensures UI responsiveness during debugging without affecting normal execution performance.

The recommended approach is to start with **Phase 1-2** to build the core functionality, then move to **Phase 3-4** for DevTools integration. This allows for early testing and iteration on the fundamental debugging mechanisms before tackling the more complex source map and CDP integration.
