# Node.js to Chrome DevTools Integration

## Overview

When you debug a Node.js application using Chrome DevTools, you're actually connecting two separate processes through a standardized protocol. This document explains the architecture, protocols, and practical implementation details for enabling this connection with your Scheme interpreter.

---

## The Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Chrome Browser                          │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Chrome DevTools Frontend                 │  │
│  │  (UI: Sources panel, Console, Debugger controls)     │  │
│  └────────────────────┬─────────────────────────────────┘  │
│                       │                                     │
│                       │ Chrome DevTools Protocol (CDP)      │
│                       │ over WebSocket                      │
└───────────────────────┼─────────────────────────────────────┘
                        │
                        │ ws://localhost:9229/...
                        │
┌───────────────────────┼─────────────────────────────────────┐
│                       │          Node.js Process            │
│  ┌────────────────────▼─────────────────────────────────┐  │
│  │         Inspector (CDP Server)                       │  │
│  │  - Handles CDP messages                              │  │
│  │  - Manages breakpoints                               │  │
│  │  - Controls execution                                │  │
│  └────────────────────┬─────────────────────────────────┘  │
│                       │                                     │
│  ┌────────────────────▼─────────────────────────────────┐  │
│  │         V8 JavaScript Engine                         │  │
│  │  - Executes JavaScript                               │  │
│  │  - Provides debugging hooks                          │  │
│  │  - Generates events (paused, resumed, etc.)          │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │         Your Scheme Interpreter                      │  │
│  │  - Runs on top of V8                                 │  │
│  │  - Generates source maps                             │  │
│  │  - Hooks into Inspector API                          │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

---

## How the Connection Works

### Step 1: Starting the Inspector

When you start Node.js with debugging enabled:

```bash
# Start Node.js with inspector on port 9229
node --inspect server.js

# Or use --inspect-brk to break on first line
node --inspect-brk server.js

# Specify custom port
node --inspect=9230 server.js
```

Node.js outputs:
```
Debugger listening on ws://127.0.0.1:9229/5f8a1b2c-3d4e-5f6a-7b8c-9d0e1f2a3b4c
For help, see: https://nodejs.org/en/docs/inspector
```

This message contains:
- **WebSocket URL**: `ws://127.0.0.1:9229/...`
- **Session UUID**: `5f8a1b2c-3d4e-5f6a-7b8c-9d0e1f2a3b4c`

### Step 2: Chrome DevTools Discovery

Chrome DevTools needs to discover available debug targets. It does this through an HTTP endpoint:

```bash
# Query the inspector endpoint
curl http://localhost:9229/json/list
```

Response:
```json
[
  {
    "description": "node.js instance",
    "devtoolsFrontendUrl": "devtools://devtools/bundled/js_app.html?experiments=true&v8only=true&ws=127.0.0.1:9229/5f8a1b2c-3d4e-5f6a-7b8c-9d0e1f2a3b4c",
    "devtoolsFrontendUrlCompat": "devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=127.0.0.1:9229/5f8a1b2c-3d4e-5f6a-7b8c-9d0e1f2a3b4c",
    "faviconUrl": "https://nodejs.org/static/images/favicons/favicon.png",
    "id": "5f8a1b2c-3d4e-5f6a-7b8c-9d0e1f2a3b4c",
    "title": "server.js",
    "type": "node",
    "url": "file:///path/to/server.js",
    "webSocketDebuggerUrl": "ws://127.0.0.1:9229/5f8a1b2c-3d4e-5f6a-7b8c-9d0e1f2a3b4c"
  }
]
```

### Step 3: Opening DevTools

When you open Chrome DevTools for Node.js, you can:

**Option 1: Use chrome://inspect**
1. Navigate to `chrome://inspect` in Chrome
2. Chrome polls `http://localhost:9229/json` for targets
3. Click "inspect" next to your Node.js process
4. Chrome opens DevTools and connects via WebSocket

**Option 2: Use the devtools:// URL directly**
```
devtools://devtools/bundled/js_app.html?experiments=true&v8only=true&ws=127.0.0.1:9229/5f8a1b2c-3d4e-5f6a-7b8c-9d0e1f2a3b4c
```

### Step 4: WebSocket Connection

Once DevTools opens, it establishes a WebSocket connection:

```
Client (Chrome DevTools)           Server (Node.js Inspector)
         │                                    │
         │──── WebSocket Handshake ──────────▶│
         │                                    │
         │◀─── 101 Switching Protocols ───────│
         │                                    │
         │──── CDP Message (JSON-RPC) ────────▶│
         │     { id: 1, method: "Debugger.   │
         │       enable", params: {} }        │
         │                                    │
         │◀─── CDP Response ──────────────────│
         │     { id: 1, result: {} }          │
         │                                    │
         │◀─── CDP Event ─────────────────────│
         │     { method: "Debugger.paused",   │
         │       params: { ... } }            │
```

---

## Chrome DevTools Protocol (CDP)

CDP is a JSON-RPC 2.0 protocol over WebSocket. Messages have this structure:

### Request Format
```json
{
  "id": 1,
  "method": "Domain.method",
  "params": {
    "param1": "value1"
  }
}
```

### Response Format
```json
{
  "id": 1,
  "result": {
    "returnValue": "data"
  }
}
```

### Event Format (Server → Client)
```json
{
  "method": "Domain.event",
  "params": {
    "data": "value"
  }
}
```

### Key CDP Domains for Debugging

**Debugger Domain** - Core debugging operations:
- `Debugger.enable` - Enable debugging
- `Debugger.setBreakpoint` - Set breakpoint
- `Debugger.pause` - Pause execution
- `Debugger.resume` - Resume execution
- `Debugger.stepInto` - Step into function
- `Debugger.stepOver` - Step over line
- `Debugger.stepOut` - Step out of function
- `Debugger.paused` (event) - Execution paused
- `Debugger.resumed` (event) - Execution resumed
- `Debugger.scriptParsed` (event) - New script loaded

**Runtime Domain** - Execution and evaluation:
- `Runtime.evaluate` - Evaluate expression
- `Runtime.getProperties` - Get object properties
- `Runtime.callFunctionOn` - Call function on object

**Profiler Domain** - Performance profiling:
- `Profiler.enable` - Enable profiling
- `Profiler.start` - Start CPU profiling
- `Profiler.stop` - Stop CPU profiling

---

## Implementation for Your Scheme Interpreter

### Architecture Decision: Two Approaches

#### **Approach 1: Native Inspector Integration (Recommended for Node.js)**

Use Node.js's built-in Inspector API directly:

```javascript
// scheme-node-debugger.js
const inspector = require('inspector');
const { EventEmitter } = require('events');

class SchemeNodeDebugger extends EventEmitter {
  constructor(interpreter) {
    super();
    this.interpreter = interpreter;
    this.session = new inspector.Session();
    this.session.connect();
    
    this.scriptIdMap = new Map(); // Scheme file → CDP scriptId
    this.breakpoints = new Map();
    
    this.setupInspectorListeners();
  }
  
  setupInspectorListeners() {
    // Listen to CDP events from Node.js Inspector
    this.session.on('Debugger.paused', (message) => {
      this.handlePaused(message);
    });
    
    this.session.on('Debugger.resumed', (message) => {
      this.handleResumed(message);
    });
    
    // Forward events to interpreter's debug runtime
    this.on('paused', (data) => {
      this.interpreter.debugRuntime?.handlePause(data);
    });
  }
  
  enableDebugging() {
    // Enable the Debugger domain
    return new Promise((resolve, reject) => {
      this.session.post('Debugger.enable', {}, (err, result) => {
        if (err) reject(err);
        else resolve(result);
      });
    });
  }
  
  registerSchemeScript(filename, sourceCode, compiledJS, sourceMap) {
    // When Scheme code is compiled to JS, register it with Inspector
    
    // 1. Create a temporary file or use eval with sourceURL
    const wrappedCode = `${compiledJS}\n//# sourceURL=${filename}`;
    
    // 2. Execute it so V8 knows about it
    const script = new vm.Script(wrappedCode, { filename });
    
    // 3. This triggers 'Debugger.scriptParsed' event
    this.session.once('Debugger.scriptParsed', (params) => {
      const scriptId = params.scriptId;
      this.scriptIdMap.set(filename, scriptId);
      
      // 4. Set source map
      if (sourceMap) {
        this.session.post('Debugger.setScriptSource', {
          scriptId,
          scriptSource: JSON.stringify(sourceMap)
        });
      }
    });
    
    return script;
  }
  
  setBreakpoint(filename, line, column = 0) {
    return new Promise((resolve, reject) => {
      // Get the scriptId for this Scheme file
      const scriptId = this.scriptIdMap.get(filename);
      
      if (!scriptId) {
        reject(new Error(`Script not found: ${filename}`));
        return;
      }
      
      // Set breakpoint via CDP
      this.session.post('Debugger.setBreakpoint', {
        location: {
          scriptId,
          lineNumber: line - 1, // CDP uses 0-based lines
          columnNumber: column
        }
      }, (err, result) => {
        if (err) {
          reject(err);
        } else {
          const breakpointId = result.breakpointId;
          this.breakpoints.set(breakpointId, { filename, line, column });
          resolve(breakpointId);
        }
      });
    });
  }
  
  removeBreakpoint(breakpointId) {
    return new Promise((resolve, reject) => {
      this.session.post('Debugger.removeBreakpoint', {
        breakpointId
      }, (err) => {
        if (err) reject(err);
        else {
          this.breakpoints.delete(breakpointId);
          resolve();
        }
      });
    });
  }
  
  pause() {
    return new Promise((resolve, reject) => {
      this.session.post('Debugger.pause', {}, (err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  }
  
  resume() {
    return new Promise((resolve, reject) => {
      this.session.post('Debugger.resume', {}, (err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  }
  
  stepOver() {
    return new Promise((resolve, reject) => {
      this.session.post('Debugger.stepOver', {}, (err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  }
  
  stepInto() {
    return new Promise((resolve, reject) => {
      this.session.post('Debugger.stepInto', {}, (err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  }
  
  stepOut() {
    return new Promise((resolve, reject) => {
      this.session.post('Debugger.stepOut', {}, (err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  }
  
  evaluateOnCallFrame(callFrameId, expression) {
    return new Promise((resolve, reject) => {
      this.session.post('Debugger.evaluateOnCallFrame', {
        callFrameId,
        expression
      }, (err, result) => {
        if (err) reject(err);
        else resolve(result.result);
      });
    });
  }
  
  handlePaused(message) {
    const { callFrames, reason, data } = message.params;
    
    // Translate CDP call frames to Scheme frames
    const schemeFrames = this.translateCallFrames(callFrames);
    
    this.emit('paused', {
      callFrames: schemeFrames,
      reason,
      data
    });
  }
  
  translateCallFrames(cdpFrames) {
    return cdpFrames.map(frame => {
      // Check if this frame is from Scheme code
      const filename = this.getFilenameFromScriptId(frame.location.scriptId);
      
      return {
        callFrameId: frame.callFrameId,
        functionName: frame.functionName || '<anonymous>',
        location: {
          filename,
          line: frame.location.lineNumber + 1, // Convert back to 1-based
          column: frame.location.columnNumber
        },
        scopeChain: frame.scopeChain,
        this: frame.this,
        returnValue: frame.returnValue,
        isScheme: filename?.endsWith('.scm') || filename?.endsWith('.sld')
      };
    });
  }
  
  getFilenameFromScriptId(scriptId) {
    for (const [filename, id] of this.scriptIdMap.entries()) {
      if (id === scriptId) return filename;
    }
    return null;
  }
  
  handleResumed(message) {
    this.emit('resumed');
  }
}

module.exports = SchemeNodeDebugger;
```

#### **Approach 2: Custom CDP Server (For Full Control)**

If you need more control or want to implement debugging features not supported by V8:

```javascript
// custom-cdp-server.js
const WebSocket = require('ws');
const http = require('http');
const { v4: uuidv4 } = require('uuid');

class CustomCDPServer {
  constructor(interpreter, port = 9229) {
    this.interpreter = interpreter;
    this.port = port;
    this.sessionId = uuidv4();
    this.httpServer = null;
    this.wsServer = null;
    this.connections = new Set();
    
    this.messageId = 0;
    this.pendingRequests = new Map();
  }
  
  start() {
    // Create HTTP server for discovery endpoint
    this.httpServer = http.createServer((req, res) => {
      if (req.url === '/json' || req.url === '/json/list') {
        this.handleDiscovery(req, res);
      } else if (req.url === '/json/version') {
        this.handleVersion(req, res);
      } else {
        res.writeHead(404);
        res.end();
      }
    });
    
    // Create WebSocket server for CDP
    this.wsServer = new WebSocket.Server({
      server: this.httpServer,
      path: `/${this.sessionId}`
    });
    
    this.wsServer.on('connection', (ws) => {
      this.handleConnection(ws);
    });
    
    this.httpServer.listen(this.port, () => {
      console.log(`Debugger listening on ws://127.0.0.1:${this.port}/${this.sessionId}`);
      console.log(`For help, see: https://nodejs.org/en/docs/inspector`);
    });
  }
  
  handleDiscovery(req, res) {
    const response = [{
      description: 'scheme.js instance',
      devtoolsFrontendUrl: `devtools://devtools/bundled/js_app.html?experiments=true&v8only=true&ws=127.0.0.1:${this.port}/${this.sessionId}`,
      devtoolsFrontendUrlCompat: `devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=127.0.0.1:${this.port}/${this.sessionId}`,
      faviconUrl: 'https://example.com/scheme-icon.png',
      id: this.sessionId,
      title: 'scheme.js',
      type: 'node',
      url: 'file:///scheme-app.scm',
      webSocketDebuggerUrl: `ws://127.0.0.1:${this.port}/${this.sessionId}`
    }];
    
    res.writeHead(200, {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*'
    });
    res.end(JSON.stringify(response));
  }
  
  handleVersion(req, res) {
    const response = {
      Browser: 'scheme.js/1.0',
      'Protocol-Version': '1.3'
    };
    
    res.writeHead(200, {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*'
    });
    res.end(JSON.stringify(response));
  }
  
  handleConnection(ws) {
    console.log('DevTools connected');
    this.connections.add(ws);
    
    ws.on('message', (data) => {
      try {
        const message = JSON.parse(data);
        this.handleCDPMessage(ws, message);
      } catch (err) {
        console.error('Invalid CDP message:', err);
      }
    });
    
    ws.on('close', () => {
      console.log('DevTools disconnected');
      this.connections.delete(ws);
    });
  }
  
  handleCDPMessage(ws, message) {
    const { id, method, params } = message;
    
    // Route to appropriate handler
    const [domain, methodName] = method.split('.');
    
    const handler = this.handlers[domain]?.[methodName];
    if (handler) {
      handler.call(this, params, (error, result) => {
        if (error) {
          this.sendError(ws, id, error);
        } else {
          this.sendResponse(ws, id, result || {});
        }
      });
    } else {
      this.sendError(ws, id, {
        code: -32601,
        message: `Method not found: ${method}`
      });
    }
  }
  
  handlers = {
    Debugger: {
      enable: function(params, callback) {
        this.interpreter.debugRuntime.enable();
        callback(null, {
          debuggerId: this.sessionId
        });
      },
      
      disable: function(params, callback) {
        this.interpreter.debugRuntime.disable();
        callback(null, {});
      },
      
      setBreakpointByUrl: function(params, callback) {
        const { lineNumber, url, columnNumber, condition } = params;
        
        const breakpoint = this.interpreter.debugRuntime.setBreakpoint(
          url,
          lineNumber + 1, // Convert to 1-based
          columnNumber,
          condition
        );
        
        callback(null, {
          breakpointId: breakpoint.id,
          locations: [{
            scriptId: breakpoint.scriptId,
            lineNumber: lineNumber,
            columnNumber: columnNumber || 0
          }]
        });
      },
      
      removeBreakpoint: function(params, callback) {
        const { breakpointId } = params;
        this.interpreter.debugRuntime.removeBreakpoint(breakpointId);
        callback(null, {});
      },
      
      pause: function(params, callback) {
        this.interpreter.debugRuntime.pause();
        callback(null, {});
      },
      
      resume: function(params, callback) {
        this.interpreter.debugRuntime.resume();
        callback(null, {});
      },
      
      stepOver: function(params, callback) {
        this.interpreter.debugRuntime.stepOver();
        callback(null, {});
      },
      
      stepInto: function(params, callback) {
        this.interpreter.debugRuntime.stepInto();
        callback(null, {});
      },
      
      stepOut: function(params, callback) {
        this.interpreter.debugRuntime.stepOut();
        callback(null, {});
      },
      
      evaluateOnCallFrame: function(params, callback) {
        const { callFrameId, expression } = params;
        
        try {
          const result = this.interpreter.debugRuntime.evaluateOnCallFrame(
            callFrameId,
            expression
          );
          
          callback(null, {
            result: this.serializeValue(result)
          });
        } catch (err) {
          callback(null, {
            exceptionDetails: {
              text: err.message,
              exception: this.serializeValue(err)
            }
          });
        }
      },
      
      getPossibleBreakpoints: function(params, callback) {
        const { start, end } = params;
        
        const breakpoints = this.interpreter.debugRuntime.getPossibleBreakpoints(
          start.scriptId,
          start.lineNumber,
          end?.lineNumber
        );
        
        callback(null, { locations: breakpoints });
      }
    },
    
    Runtime: {
      enable: function(params, callback) {
        callback(null, {});
      },
      
      getProperties: function(params, callback) {
        const { objectId, ownProperties } = params;
        
        const properties = this.interpreter.debugRuntime.getObjectProperties(
          objectId,
          ownProperties
        );
        
        callback(null, {
          result: properties.map(prop => ({
            name: prop.name,
            value: this.serializeValue(prop.value),
            writable: prop.writable,
            configurable: prop.configurable,
            enumerable: prop.enumerable
          }))
        });
      },
      
      evaluate: function(params, callback) {
        const { expression, contextId } = params;
        
        try {
          const result = this.interpreter.debugRuntime.evaluate(
            expression,
            contextId
          );
          
          callback(null, {
            result: this.serializeValue(result)
          });
        } catch (err) {
          callback(null, {
            exceptionDetails: {
              text: err.message,
              exception: this.serializeValue(err)
            }
          });
        }
      }
    },
    
    Profiler: {
      enable: function(params, callback) {
        callback(null, {});
      },
      
      disable: function(params, callback) {
        callback(null, {});
      }
    }
  };
  
  serializeValue(value) {
    if (value === null) {
      return { type: 'object', subtype: 'null', value: null };
    }
    if (value === undefined) {
      return { type: 'undefined' };
    }
    if (typeof value === 'number') {
      return { type: 'number', value };
    }
    if (typeof value === 'string') {
      return { type: 'string', value };
    }
    if (typeof value === 'boolean') {
      return { type: 'boolean', value };
    }
    if (typeof value === 'function') {
      return {
        type: 'function',
        className: 'Function',
        description: value.toString(),
        objectId: this.createObjectId(value)
      };
    }
    if (Array.isArray(value)) {
      return {
        type: 'object',
        subtype: 'array',
        className: 'Array',
        description: `Array(${value.length})`,
        objectId: this.createObjectId(value)
      };
    }
    // Default object
    return {
      type: 'object',
      className: 'Object',
      description: 'Object',
      objectId: this.createObjectId(value)
    };
  }
  
  createObjectId(obj) {
    // Create a unique ID for object references
    const id = `object:${this.messageId++}`;
    this.objectStore.set(id, obj);
    return id;
  }
  
  sendResponse(ws, id, result) {
    ws.send(JSON.stringify({ id, result }));
  }
  
  sendError(ws, id, error) {
    ws.send(JSON.stringify({ id, error }));
  }
  
  sendEvent(method, params) {
    const message = JSON.stringify({ method, params });
    for (const ws of this.connections) {
      ws.send(message);
    }
  }
  
  // Called by your debug runtime when execution pauses
  notifyPaused(reason, callFrames, data) {
    this.sendEvent('Debugger.paused', {
      reason,
      callFrames: callFrames.map(frame => ({
        callFrameId: frame.id,
        functionName: frame.functionName,
        location: {
          scriptId: frame.scriptId,
          lineNumber: frame.line - 1, // Convert to 0-based
          columnNumber: frame.column
        },
        url: frame.url,
        scopeChain: frame.scopeChain.map(scope => ({
          type: scope.type,
          object: this.serializeValue(scope.object),
          name: scope.name
        })),
        this: this.serializeValue(null)
      })),
      data
    });
  }
  
  // Called when execution resumes
  notifyResumed() {
    this.sendEvent('Debugger.resumed', {});
  }
  
  // Called when a new script is parsed
  notifyScriptParsed(scriptId, url, sourceMapURL) {
    this.sendEvent('Debugger.scriptParsed', {
      scriptId,
      url,
      startLine: 0,
      startColumn: 0,
      endLine: 1000, // Could be more precise
      endColumn: 0,
      executionContextId: 1,
      hash: '',
      sourceMapURL,
      hasSourceURL: true,
      isModule: false,
      length: 0,
      scriptLanguage: 'Scheme'
    });
  }
  
  stop() {
    this.wsServer.close();
    this.httpServer.close();
  }
}

module.exports = CustomCDPServer;
```

---

## Usage Examples

### Starting Your Scheme Debugger in Node.js

```javascript
// scheme-app.js
const SchemeInterpreter = require('./scheme-interpreter');
const SchemeNodeDebugger = require('./scheme-node-debugger');

const interpreter = new SchemeInterpreter();
const debugger = new SchemeNodeDebugger(interpreter);

// Enable debugging
await debugger.enableDebugging();

// Load and run Scheme code
const schemeCode = `
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
`;

const script = debugger.registerSchemeScript(
  'factorial.scm',
  schemeCode,
  compiledJS,
  sourceMap
);

// Set a breakpoint
await debugger.setBreakpoint('factorial.scm', 3);

// Run the script (will pause at breakpoint)
script.runInThisContext();
```

Then start with:
```bash
node --inspect scheme-app.js
```

Open Chrome, go to `chrome://inspect`, and click "inspect" on your process.

### Using the Custom CDP Server

```javascript
// scheme-app.js
const SchemeInterpreter = require('./scheme-interpreter');
const CustomCDPServer = require('./custom-cdp-server');

const interpreter = new SchemeInterpreter();
const server = new CustomCDPServer(interpreter, 9229);

server.start();

// Load Scheme code
// ... rest of your app
```

Start with:
```bash
node scheme-app.js
```

---

## Source Map Handling

Both approaches need to handle source maps properly:

```javascript
class SourceMapHandler {
  constructor(cdpServer) {
    this.cdpServer = cdpServer;
    this.sourceMaps = new Map();
  }
  
  registerSourceMap(scriptId, sourceMap) {
    this.sourceMaps.set(scriptId, sourceMap);
    
    // Notify DevTools about the source map
    const sourceMapURL = this.createSourceMapDataURL(sourceMap);
    
    this.cdpServer.notifyScriptParsed(
      scriptId,
      sourceMap.file,
      sourceMapURL
    );
  }
  
  createSourceMapDataURL(sourceMap) {
    const json = JSON.stringify(sourceMap);
    const base64 = Buffer.from(json).toString('base64');
    return `data:application/json;charset=utf-8;base64,${base64}`;
  }
  
  translateLocation(scriptId, line, column) {
    const sourceMap = this.sourceMaps.get(scriptId);
    if (!sourceMap) return { line, column };
    
    // Use source-map library to translate
    const consumer = new SourceMapConsumer(sourceMap);
    const original = consumer.originalPositionFor({
      line,
      column
    });
    
    return {
      source: original.source,
      line: original.line,
      column: original.column,
      name: original.name
    };
  }
}
```

---

## Complete Integration Example

```javascript
// complete-scheme-debugger.js
const inspector = require('inspector');
const { SourceMapConsumer } = require('source-map');

class CompleteSchemeDebugger {
  constructor(interpreter) {
    this.interpreter = interpreter;
    this.useNativeInspector = true; // or false for custom server
    
    if (this.useNativeInspector) {
      this.backend = new NativeInspectorBackend(this);
    } else {
      this.backend = new CustomCDPServer(this, 9229);
    }
    
    this.sourceMapHandler = new SourceMapHandler(this.backend);
    this.breakpointManager = new BreakpointManager();
    this.callStackManager = new CallStackManager();
  }
  
  async start() {
    await this.backend.start();
    
    // Register interpreter hooks
    this.interpreter.on('compile', (file, source, compiled, sourceMap) => {
      const scriptId = this.backend.registerScript(file, compiled);
      this.sourceMapHandler.registerSourceMap(scriptId, sourceMap);
    });
    
    this.interpreter.on('pause', (location, callFrames) => {
      this.backend.notifyPaused('breakpoint', callFrames);
    });
    
    this.interpreter.on('resume', () => {
      this.backend.notifyResumed();
    });
  }
  
  async setBreakpoint(file, line, column) {
    return this.breakpointManager.setBreakpoint(file, line, column);
  }
  
  async removeBreakpoint(breakpointId) {
    return this.breakpointManager.removeBreakpoint(breakpointId);
  }
  
  async pause() {
    return this.backend.pause();
  }
  
  async resume() {
    return this.backend.resume();
  }
  
  async stepOver() {
    return this.backend.stepOver();
  }
  
  async stepInto() {
    return this.backend.stepInto();
  }
  
  async stepOut() {
    return this.backend.stepOut();
  }
}

// Usage
const interpreter = new SchemeInterpreter();
const debugger = new CompleteSchemeDebugger(interpreter);

await debugger.start();

// Now you can use Chrome DevTools!
```

---

## Key Differences: Browser vs Node.js

| Aspect | Browser | Node.js |
|--------|---------|---------|
| **Connection** | DevTools embedded in same process | WebSocket over network |
| **Discovery** | Automatic (same browser) | Requires `chrome://inspect` |
| **Protocol** | CDP over internal IPC | CDP over WebSocket |
| **Inspector** | Built into browser | Separate inspector thread |
| **Source Maps** | Inline or external HTTP | Inline or file:// URLs |
| **Security** | Same-origin policy | Network exposed (localhost) |

---

## Best Practices

1. **Use Native Inspector When Possible**: It's more robust and maintained by Node.js team
2. **Implement Source Maps Correctly**: Essential for good debugging experience
3. **Handle Async Carefully**: Inspector API is callback-based, promisify it
4. **Test with Real DevTools**: Don't rely solely on mocking
5. **Document Protocol Limitations**: Not all CDP features work everywhere
6. **Provide Fallbacks**: Custom server for advanced features not in V8
7. **Security**: Only bind inspector to localhost in production

---

## Troubleshooting

### DevTools Can't Connect
- Check firewall settings
- Verify WebSocket URL is correct
- Ensure inspector is listening: `curl http://localhost:9229/json`

### Breakpoints Not Working
- Verify source maps are correct
- Check that scriptId mapping is accurate
- Ensure breakpoints are set after script loads

### Source Maps Not Applied
- Check source map format (must be v3)
- Verify sourceMapURL is accessible
- Use inline source maps for reliability

### Performance Issues
- Inspector adds overhead (~10-30%)
- Use conditional compilation for production
- Disable profiler when not needed

---

## Summary

To integrate your Scheme interpreter with Chrome DevTools in Node.js:

1. **Use Node.js Inspector API** for the easiest path (Approach 1)
2. **Generate proper source maps** from Scheme to JavaScript
3. **Register scripts** with Inspector as they're compiled
4. **Handle CDP messages** for debugging operations
5. **Start Node.js with `--inspect`** flag
6. **Connect via `chrome://inspect`** in Chrome

The native Inspector approach leverages V8's built-in debugging infrastructure, providing a robust foundation while allowing you to add Scheme-specific features through source maps and metadata.
