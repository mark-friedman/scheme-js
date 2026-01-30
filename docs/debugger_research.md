# **The Engineering of Transparent Execution: Debugging Mechanisms for Non-JavaScript Browser Runtimes**

The transition of the web browser from a document-retrieval system to a high-performance execution environment for polyglot applications has necessitated the development of sophisticated debugging abstractions. While JavaScript remains the native language of the browser's execution engines—specifically the V8, SpiderMonkey, and JavaScriptCore engines—a significant ecosystem of non-JavaScript languages has emerged to allow developers to leverage the syntax and libraries of languages like Python directly within the web client.1 Frameworks such as Brython, Skulpt, and PyScript enable this by providing runtimes that reside entirely within the browser's sandbox. However, the inherent abstraction between the guest language (e.g., Python) and the host environment (JavaScript) creates a transparency gap. Bridging this gap to provide features such as line-of-code breakpoints, single-stepping, and variable inspection requires a multi-layered coordination between language-specific interpreters, the Source Map Revision 3 protocol, and the Chrome DevTools Protocol (CDP).4

The engineering challenge is twofold: first, the runtime must maintain a representation of the guest language's state that is accessible to the browser; second, the browser's native debugging tools must be instructed on how to map their low-level JavaScript execution data back to the high-level source code of the guest language.4 This report explores the mechanisms by which JS-implemented interpreters reify execution state, the technical intricacies of dynamic source mapping, and the architectural patterns used to simulate synchronous debugging operations in an inherently asynchronous event loop.8

## **Architectural Patterns of Browser-Based Interpreters**

To understand how debugging is enabled, one must first categorize the execution models of non-JS languages. Most browser-based implementations follow either a transpilation-to-JavaScript model or a virtual-machine-in-JavaScript model.

### **The Transpilation Model: Brython**

Brython operates as a source-to-source translator, converting Python 3 code into an equivalent JavaScript representation.1 This approach allows the browser's native JIT (Just-In-Time) compiler to optimize the code, but it complicates the debugging process because the executing code bears little structural resemblance to the original Python.1 To mitigate this, Brython utilizes a global metadata object, \_\_BRYTHON\_\_, which acts as a central repository for module states, stack frames, and the translation engine.1

The implementation of debugging in Brython is largely controlled through the brython() initialization function, which sets the debug\_mode. This mode determines the granularity of information exposed to the browser console and the internal logging mechanisms.12

| Debug Level | Technical Mechanism | Observable Behavior |
| :---- | :---- | :---- |
| Level 0/1 | Basic exception handling and stderr redirection. | Runtime errors are caught and formatted as standard Python tracebacks in the console.12 |
| Level 2 | Transpilation logging. | The generated JavaScript code for the main script is printed to the console before execution.12 |
| Level 10 | Comprehensive library logging. | Both the main script and all imported standard library modules have their generated JS printed to the console.12 |

When an error occurs at Level 1, Brython’s runtime catches the JavaScript exception and walks the call stack. By cross-referencing the JS stack frames with the metadata stored in \_\_BRYTHON\_\_.modules, the runtime can reconstruct a Python-style traceback that points to the correct line in the original script tag or external .py file.1

### **The Virtual Machine Model: Skulpt**

Skulpt takes a different approach by implementing a Python Virtual Machine (VM) directly in JavaScript.8 Instead of translating Python logic into JS logic, Skulpt parses Python into an Abstract Syntax Tree (AST) and then executes that AST node-by-node using a JS-based interpreter.14 This model provides superior control over execution state but suffers from performance overhead compared to direct transpilation.

The core debugging mechanism in Skulpt is the "suspension".8 Because the browser’s main thread must not be blocked, a long-running Python loop would normally freeze the UI. A suspension is an object that captures the entire state of the Skulpt VM—including the call stack, the instruction pointer, and the local variable scope—allowing the interpreter to "pause" itself and yield control back to the browser’s event loop.8

A suspension ![][image1] at instruction ![][image2] can be conceptualized as:

![][image3]  
By passing a debugging: true flag to the Skulpt compiler, the engine is instructed to generate a suspension before the execution of every statement.8 This effectively creates a granular "hook" that the debugger can use to implement single-stepping. A breakpoints() callback allows the user to filter these suspensions, pausing only when the instruction pointer matches a line number where a breakpoint has been set in the UI.8

## **Source Maps: The Linkage Protocol**

Regardless of whether the language uses transpilation or a VM, the primary interface for the developer remains the browser's "Sources" panel. The Source Map Revision 3 protocol is the standard that enables this integration.4

### **Technical Structure of Source Maps**

A source map is a JSON object that defines the relationship between a "generated" file (the JavaScript actually running) and "original" files (the Python source).4 For browser-based languages, this map must often be generated dynamically on the client side.6

| Field | Description | Role in Debugging |
| :---- | :---- | :---- |
| mappings | A string of Base64 VLQ-encoded segments. | Provides the coordinate mapping between JS and the guest language.5 |
| sources | Array of URLs/filenames for the original source. | Populates the file navigator in the Sources tool.4 |
| sourcesContent | Array containing the raw text of the original source. | Allows debugging of code that does not exist as a physical file on the server.5 |
| names | Array of symbol names. | Maps minified or mangled JS variable names back to original Python names.5 |

The mappings field is the most complex. It consists of groups of segments separated by semicolons (representing lines in the generated JS) and commas (representing segments within a line).6 Each segment uses Variable Length Quantity (VLQ) encoding to store relative offsets. This allows the browser to perform an efficient ![][image4] or ![][image5] lookup to find which Python line corresponds to the currently executing JavaScript line.5

### **Dynamic Source Mapping for Evaluated Code**

A unique challenge for browser scripting is that code is often evaluated dynamically (e.g., via a REPL or an inline \<script type="text/python"\>). In these cases, there is no static file on the server for the browser to fetch.6 Developers solve this using the sourceURL and sourceMappingURL directives.6

When an interpreter like Brython or a CoffeeScript compiler processes a string of code, it generates the corresponding JavaScript and a source map. To make this "visible" to DevTools, it appends two comments to the end of the JS string:

1. //\# sourceURL=my\_script.py: This tells the browser to treat this evaluated string as a virtual file named my\_script.py.7
2. //\# sourceMappingURL=data:application/json;base64,...: This provides the source map as a Base64-encoded data URI.6

This mechanism allows the browser's debugger to pause execution on an eval() call and show the developer the original Python source, complete with the ability to set breakpoints that persist as long as the sourceURL remains the same.6

## **Variable Inspection and State Reification**

Variable inspection requires the interpreter to expose its internal data structures in a format that the browser's "Scope" and "Watch" panes can interpret.

### **Proxies and Foreign Function Interfaces (FFI)**

Languages implemented in JavaScript often use Proxy objects to bridge the gap between guest and host objects. PyScript, which builds upon Pyodide, utilizes a robust FFI that allows Python objects to be accessed from JavaScript as if they were native JS objects.2

When a developer inspects a Python variable in the DevTools console, they are often interacting with a JS Proxy. This proxy intercepts calls to properties (like .length or \[index\]) and translates them into Python-specific lookups (like \_\_len\_\_ or \_\_getitem\_\_).1 For the purposes of variable inspection:

* **The globals object:** Pyodide exposes the Python global scope via pyodide.globals. DevTools can traverse this object to show all variables currently defined in the Python script.18
* **The locals object:** During a suspension (in Skulpt) or a paused breakpoint, the interpreter provides the current frame's local variable map to the debugger.8

### **Handling Exceptions and Stack Traces**

Effective debugging requires that exceptions thrown in the guest language be indistinguishable from native JS errors in the console. Brython and Pyodide achieve this by wrapping the guest language's exception objects in a custom JS Error subclass.13

The stack trace of a Python error in the browser typically contains two parts:

1. **The JS Stack:** The actual sequence of function calls within the interpreter (e.g., py\_int.js, py\_dicts.js).1
2. **The Guest Stack:** The sequence of Python function calls synthesized from the interpreter's internal metadata.1

By utilizing Source Maps, the browser automatically hides the irrelevant "JS Stack" frames (a process known as "blackboxing" or adding to the "Ignore List") and shows only the relevant Python frames in the Call Stack pane.4

## **Simulating Synchronous Debugging in an Asynchronous Environment**

The JavaScript execution model is strictly non-blocking. However, debugging features like "breakpoints" and "stepping" are conceptually synchronous: they stop the flow of time for the script.

### **Instrumentation and Yielding: Stopify**

Stopify is a sophisticated source-to-source compiler that enables these features by transforming synchronous code into an asynchronous, resumable form.9 Stopify extends standard JavaScript with debugging abstractions that are independent of the browser's native debugger.

Stopify's instrumentation works by rewriting every function call and loop. For example, a function ![][image6] is rewritten to include a state-check at every potential pause point 9:

JavaScript

function P(f, g, x) {  
var locals, reenter;  
if (mode \=== 'restore') {  
// Re-establish local variables from the saved stack frame  
k \= stack.pop();  
\[y1, y2\] \= k.locals();  
}  
// Execution logic...  
if (mode \=== 'capture') {  
// Save current state and exit the function early  
stack.push(new Frame(locals, reenter));  
return;  
}  
}

This transformation allows a "Stop" button or a "Breakpoint" to function even during a tight computational loop that would otherwise hang the browser. The system periodically yields control to the browser using setTimeout or requestAnimationFrame, checks if a "Pause" command has been issued, and then "resumes" the execution by re-entering the function and jumping to the correct instruction.9

### **Stepping Logic: Into, Over, and Out**

The implementation of "stepping" in an interpreted environment like Skulpt or an instrumented one like Stopify depends on the manipulation of the call stack.

| Operation | Implementation Logic |
| :---- | :---- |
| **Step Into** | Pause at the very next suspension or AST node, regardless of function boundaries.14 |
| **Step Over** | Record the current stack depth. Set a "temporary breakpoint" at the next instruction in the current frame. Resume execution until that breakpoint is hit or an exception occurs.15 |
| **Step Out** | Record the current stack depth. Resume execution and pause only after the stack depth has decreased (i.e., the current function has returned).22 |

These operations are managed by the interpreter's internal "service" or "debug manager," which maintains the state of the debugging session (continue, step-into, or step-over modes).14

## **Interoperability with the DOM and Web APIs**

Debugging a script that manipulates the DOM involves a complex interplay between the guest language's VM and the browser's native C++ implementation of the DOM.

### **DOM Breakpoints and Event Listeners**

Browser DevTools allow developers to set "DOM Breakpoints" (e.g., "Break on Subtree Modification").23 When a Python script (via Brython or PyScript) modifies an element's attribute, the browser's native engine triggers the breakpoint. The execution pauses in the *JavaScript glue code* that performed the modification.21

The challenge is that the Call Stack at this point will show the internal interpreter functions. To make this useful, the interpreter must ensure that its source maps are correctly configured so that the developer is "jumped" back to the Python line that initiated the DOM change.4

### **The Fetch API and Network Debugging**

Network requests made via Python libraries (e.g., browser.ajax in Brython or urllib in Pyodide) are ultimately routed through the browser's XMLHttpRequest or fetch() APIs.1 DevTools provide "XHR/fetch breakpoints" that pause execution when a request URL matches a certain pattern.23

When such a breakpoint is hit, the debugger pauses exactly where the JS-to-network bridge is crossed. In PyScript, this usually happens within the Pyodide runtime's proxying logic.2 Because these requests are often asynchronous, the "Call Stack" pane in DevTools uses "Async Stack Traces" to link the network response back to the original Python function that initiated the fetch.4

## **Advanced Integration: DAP and IDE Debugging**

For professional development, the integration must extend beyond the browser's built-in DevTools to external Integrated Development Environments (IDEs) like VS Code or PyCharm. This is achieved through the Debug Adapter Protocol (DAP).27

### **The Role of DAP**

DAP standardizes the communication between an IDE and a debugger backend. For browser-based languages, the "debugger backend" is often a local proxy server that communicates with the browser via the Chrome DevTools Protocol (CDP).29

Code snippet

graph LR  
A \-- DAP (JSON-RPC) \--\> B  
B \-- CDP (WebSockets) \--\> C  
C \-- Interpretation \--\> D

The Debug Adapter translates IDE requests (like "Set Breakpoint at line 20") into CDP commands (like Debugger.setBreakpointByUrl). If the language is interpreted (like Skulpt), the Adapter might instead send a message to the Skulpt runtime to update its internal breakpoint registry.27

### **Variable Depth and Output Capture**

A critical feature of DAP-based debugging is the ability to inspect complex data structures without crashing the browser's memory. Debugging servers often implement "Execution Limits" to manage this.31

| Limit Category | Standard Constraint | Purpose |
| :---- | :---- | :---- |
| **Variable Depth** | Max 2 levels of nesting. | Prevents infinite recursion when inspecting self-referencing objects.31 |
| **Collection Size** | Max 50 items shown. | Ensures the UI remains responsive when dealing with large lists or dictionaries.31 |
| **String Length** | Max 256 characters before truncation. | Optimized data transfer between the browser and the IDE.31 |

These limits are enforced by the language interpreter before it serializes the state to be sent to the debugger UI.31

## **The Evolution of the Debugging Environment**

The ecosystem for non-JS browser languages is rapidly maturing, moving from basic console logging to integrated, high-fidelity debugging.

### **Browser Extensions and Custom Panels**

To provide a first-class experience, many languages provide their own DevTools extensions (e.g., React Developer Tools, Vue.js DevTools).29 A custom extension for a language like Python can:

1. Add a "Python" tab to DevTools that shows the Python-specific state (e.g., the sys.path, loaded modules, and object instances).29
2. Provide a "Visual REPL" that allows for the interactive testing of Python snippets in the context of the running page.1
3. Use chrome.devtools.panels.create to build custom UIs that can interact directly with the inspected window's memory.32

### **Security and Isolation**

Debugging untrusted code presents security risks. Interpreters like JS-Interpreter provide a sandboxed environment where the code can be executed step-by-step with total isolation from the browser's global scope.34 This is particularly useful for educational platforms or "playground" environments (like the Brython online editor), where users might execute code that would otherwise be malicious or disruptive.1

The implementation of "Execution Serialization" allows the entire state of the interpreter to be saved as a JSON string and restored later.34 This enables "time-travel debugging," where a developer can step backward through the code by reloading a previous state of the VM—a feature that is significantly harder to implement in native JavaScript runtimes.34

## **Conclusions and Future Outlook**

The technical foundation for debugging non-JavaScript browser languages is built upon a sophisticated synthesis of source-to-source transpilation, virtual machine instrumentation, and the standardized Source Map protocol. While native JavaScript debugging relies on the engine's built-in hooks, languages like Brython and Skulpt have pioneered the "reification" of execution state within the JavaScript environment itself.

The future of this field lies in three key areas:

1. **Source Map Revision 4:** The upcoming iteration of the source map protocol is expected to provide better support for scopes and variable names, reducing the reliance on custom proxies for variable inspection.5
2. **Standardization of DAP for Browsers:** As more languages target the browser, the Debug Adapter Protocol will likely become the primary method for providing a consistent debugging experience across IDEs and browser vendors.28
3. **Improved Performance of Instrumented Code:** New techniques for selective instrumentation will allow for high-speed execution until a breakpoint is hit, at which point the runtime can "de-optimize" to a more granular, debuggable state—mirroring the behavior of modern C++ and Java JIT compilers.

By abstracting the complexities of the browser's event loop and the JavaScript stack, these runtimes have transformed the web browser into a truly universal and transparent platform for polyglot software development. Through the continued evolution of CDP, DAP, and Source Maps, the boundary between the guest language and the host environment will continue to dissolve, providing developers with the visibility and control required for building the next generation of complex web applications.

#### **Works cited**

1. Brython: Python in Your Browser, accessed January 30, 2026, [https://realpython.com/brython-python-in-browser/](https://realpython.com/brython-python-in-browser/)
2. A First Look at PyScript: Python in the Web Browser, accessed January 30, 2026, [https://realpython.com/pyscript-python-in-browser/](https://realpython.com/pyscript-python-in-browser/)
3. PyScript: Python in the Browser \- DEV Community, accessed January 30, 2026, [https://dev.to/0xog\_pg/pyscript-python-in-the-browser-3dbd](https://dev.to/0xog_pg/pyscript-python-in-the-browser-3dbd)
4. Map the processed code to your original source code, for debugging \- Microsoft Edge Developer documentation, accessed January 30, 2026, [https://learn.microsoft.com/en-us/microsoft-edge/devtools/javascript/source-maps](https://learn.microsoft.com/en-us/microsoft-edge/devtools/javascript/source-maps)
5. What are source maps? | Articles \- web.dev, accessed January 30, 2026, [https://web.dev/articles/source-maps](https://web.dev/articles/source-maps)
6. Dynamic Source Maps \- kybernetikos, accessed January 30, 2026, [https://kybernetikos.github.io/jsSandbox/srcmaps/dynamic.html](https://kybernetikos.github.io/jsSandbox/srcmaps/dynamic.html)
7. Debug your original code instead of deployed with source maps | Chrome DevTools, accessed January 30, 2026, [https://developer.chrome.com/docs/devtools/javascript/source-maps](https://developer.chrome.com/docs/devtools/javascript/source-maps)
8. skulpt/doc/suspensions.txt at master · skulpt/skulpt · GitHub, accessed January 30, 2026, [https://github.com/skulpt/skulpt/blob/master/doc/suspensions.txt](https://github.com/skulpt/skulpt/blob/master/doc/suspensions.txt)
9. Putting in All the Stops: Execution Control for JavaScript \- People, accessed January 30, 2026, [https://people.csail.mit.edu/rachit/files/pubs/stopify-pldi18.pdf](https://people.csail.mit.edu/rachit/files/pubs/stopify-pldi18.pdf)
10. Putting in All the Stops: Execution Control for JavaScript \- Brown CS ACES, accessed January 30, 2026, [http://static.cs.brown.edu/\~sk/Publications/Papers/Published/bnpkg-stopify/paper.pdf](http://static.cs.brown.edu/~sk/Publications/Papers/Published/bnpkg-stopify/paper.pdf)
11. Brython, replace javascript with python \- Reddit, accessed January 30, 2026, [https://www.reddit.com/r/Python/comments/1fik9b/brython\_replace\_javascript\_with\_python/](https://www.reddit.com/r/Python/comments/1fik9b/brython_replace_javascript_with_python/)
12. Options available at page or script level \- Brython documentation, accessed January 30, 2026, [https://www.brython.info/static\_doc/3.13/en/options.html](https://www.brython.info/static_doc/3.13/en/options.html)
13. Brython documentation, accessed January 30, 2026, [https://brython.info/static\_doc/3.12/en/test.html](https://brython.info/static_doc/3.12/en/test.html)
14. The debbuggable interpreter design pattern \- UL Research Repository, accessed January 30, 2026, [https://researchrepository.ul.ie/server/api/core/bitstreams/7b24bd9b-3995-416e-9b2f-a57d58bdfcd9/content](https://researchrepository.ul.ie/server/api/core/bitstreams/7b24bd9b-3995-416e-9b2f-a57d58bdfcd9/content)
15. JavaScript debugging features \- Microsoft Edge Developer documentation, accessed January 30, 2026, [https://learn.microsoft.com/en-us/microsoft-edge/devtools/javascript/reference](https://learn.microsoft.com/en-us/microsoft-edge/devtools/javascript/reference)
16. How to use breakpoints in sourcemaps (Chrome DevTools) \- Stack Overflow, accessed January 30, 2026, [https://stackoverflow.com/questions/37126009/how-to-use-breakpoints-in-sourcemaps-chrome-devtools](https://stackoverflow.com/questions/37126009/how-to-use-breakpoints-in-sourcemaps-chrome-devtools)
17. Source map for a dynamically created function \- Stack Overflow, accessed January 30, 2026, [https://stackoverflow.com/questions/49463047/source-map-for-a-dynamically-created-function](https://stackoverflow.com/questions/49463047/source-map-for-a-dynamically-created-function)
18. Pyodide, accessed January 30, 2026, [https://pyodide.org/\_/downloads/en/0.22.1/pdf/](https://pyodide.org/_/downloads/en/0.22.1/pdf/)
19. Pyodide, accessed January 30, 2026, [https://pyodide.org/\_/downloads/en/0.21.3/pdf/](https://pyodide.org/_/downloads/en/0.21.3/pdf/)
20. Pyodide, accessed January 30, 2026, [https://pyodide.org/\_/downloads/en/0.23.1/epub/](https://pyodide.org/_/downloads/en/0.23.1/epub/)
21. JavaScript Debugging Techniques and Basic Concepts for Beginners \- Medium, accessed January 30, 2026, [https://medium.com/@AlexanderObregon/javascript-debugging-techniques-and-basic-concepts-for-beginners-1ba493c644a0](https://medium.com/@AlexanderObregon/javascript-debugging-techniques-and-basic-concepts-for-beginners-1ba493c644a0)
22. Debugging Javascript Like a Pro \- Bits and Pieces, accessed January 30, 2026, [https://blog.bitsrc.io/debugging-javascript-like-a-pro-a2e0f6c53c2e](https://blog.bitsrc.io/debugging-javascript-like-a-pro-a2e0f6c53c2e)
23. Debug JavaScript | Chrome DevTools, accessed January 30, 2026, [https://developer.chrome.com/docs/devtools/javascript](https://developer.chrome.com/docs/devtools/javascript)
24. Master the Art of Python Debugging With These Tips \- The New Stack, accessed January 30, 2026, [https://thenewstack.io/master-the-art-of-python-debugging-with-these-tips/](https://thenewstack.io/master-the-art-of-python-debugging-with-these-tips/)
25. Debugging JavaScript with Breakpoints | CodePath Web Development Cliffnotes, accessed January 30, 2026, [https://guides.codepath.org/webdev/Debugging-JavaScript-with-Breakpoints](https://guides.codepath.org/webdev/Debugging-JavaScript-with-Breakpoints)
26. Creating an Interactive Web App with PyScript and Pandas \- Turing, accessed January 30, 2026, [https://www.turing.com/kb/interactive-web-app-with-pyscript-and-pandas](https://www.turing.com/kb/interactive-web-app-with-pyscript-and-pandas)
27. Debugger Extension \- Visual Studio Code, accessed January 30, 2026, [https://code.visualstudio.com/api/extension-guides/debugger-extension](https://code.visualstudio.com/api/extension-guides/debugger-extension)
28. The Delve DAP MCP Server: Your AI's Eyes and Ears for Debugging, accessed January 30, 2026, [https://skywork.ai/skypage/en/delve-dap-mcp-server-debugging/1980889257930846208](https://skywork.ai/skypage/en/delve-dap-mcp-server-debugging/1980889257930846208)
29. Awesome tooling and resources in the Chrome DevTools & DevTools Protocol ecosystem \- GitHub, accessed January 30, 2026, [https://github.com/ChromeDevTools/awesome-chrome-devtools](https://github.com/ChromeDevTools/awesome-chrome-devtools)
30. The Ultimate Guide to Chrome DevTools MCP Servers: Empowering AI Agents, accessed January 30, 2026, [https://skywork.ai/skypage/en/chrome-devtools-ai-agents/1978326137347678208](https://skywork.ai/skypage/en/chrome-devtools-ai-agents/1978326137347678208)
31. Python Debugging via Model Context Protocol | MCP Servers \- LobeHub, accessed January 30, 2026, [https://lobehub.com/mcp/your-org-debug-mcp](https://lobehub.com/mcp/your-org-debug-mcp)
32. Create a DevTools extension, adding a custom tool tab and panel \- Microsoft Edge Developer documentation, accessed January 30, 2026, [https://learn.microsoft.com/en-us/microsoft-edge/extensions/developer-guide/devtools-extension](https://learn.microsoft.com/en-us/microsoft-edge/extensions/developer-guide/devtools-extension)
33. PyScript: Python In The Web Browser \- A JavaScript Library For Python \- Code With C, accessed January 30, 2026, [https://www.codewithc.com/pyscript-python-in-the-web-browser-a-javascript-library-for-python/](https://www.codewithc.com/pyscript-python-in-the-web-browser-a-javascript-library-for-python/)
34. Why are programmers like this? : r/ProgrammerHumor \- Reddit, accessed January 30, 2026, [https://www.reddit.com/r/ProgrammerHumor/comments/10cmtz4/why\_are\_programmers\_like\_this/](https://www.reddit.com/r/ProgrammerHumor/comments/10cmtz4/why_are_programmers_like_this/)
35. DAPLink/CMSIS-DAP & Debug Adapter Protocol · Issue \#560 \- GitHub, accessed January 30, 2026, [https://github.com/ARMmbed/DAPLink/issues/560](https://github.com/ARMmbed/DAPLink/issues/560)

[image1]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAAYCAYAAAAh8HdUAAABV0lEQVR4AdzSu0tCYRjH8WMRRdDW0NLY1FBEQUsF1dhSQ9FYSxAVVERL9B90gRqaisaWCKolqKiWlqL+AR0EdRIVxMsgfn/H86rnAoKTKM/nPOd95OE976XDauLXGk29fPksxtADxSCPvqDPC/HHAf6xiHX8YgVvCAU1nfHHEiawjS3M4QppZLxN4xR3sIEUTMR5ecYHLG/TqooIwxtJCu/wNfVT1JoOyZ2oj1sGr/A1faqIIyRwhzV04wc5+JpuKJ4iD826TL7GE7pgh3dNJar7UMMC+RwFzDtIlmumUbtSeWRJ2q1dspCsIT3EzDTA4ARB8ecUI06uzjRDQdeG5ItpKjG8wA4zk5p0sFN2tfYY4VXbr8PW2hjW1jTJaA+X0DYfk3Vt7sm6e9o9XithZtpkeAHdNzVGeX/EMB7gCtP07VSL5C/ovLR7Oi+G7jBN7mqDUTs2lQEAAP//DgHlBQAAAAZJREFUAwAY+jcxWkVCFgAAAABJRU5ErkJggg==>

[image2]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAXCAYAAADHhFVIAAAA1UlEQVR4AbSOvwtBURTHH9ktSpn8A1IGg8Uq2RiUhUGy+ZuYFTJTCpOSyWZGmUykfL731en5kcl7nc8959zPu/eeqPfj+5+M8EocYuC9X5tiswdp+JAFNuvwVc4QHZjDy0m9pWcWiAeYzNEUoQ1ZcKE/E1QN0KQtchNcSOrUnm4NuvpGdiF5ohpBGSSHZBeSW6oL6LoNWT3Js4EydHkYgE5XyCZLNHcYQxVUmzyyoUFq5CQsweSUpgs76MMVTJ5pJrCCA7jQtK5g0bWC0o+g9HcCa0jyCQAA///CCpLFAAAABklEQVQDABRKIS8zWosKAAAAAElFTkSuQmCC>

[image3]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAmwAAAAvCAYAAABexpbOAAAQAElEQVR4AezdA5glTdIF4Lu7v23btm3btm3btm3btm3btrnn7WdyNqe6WrN9e251n++pmKrKykqcjIo4GZm3v/vu+l8RKAJFoAgUgSJQBIrAQSNQwnbQw9PGFYEiUAS2gkDbWQSKwD4RKGHbJ7otuwgUgSJQBIpAESgCl4BACdslgNgitoFAW1kEikARKAJFYKsIlLBtdeTa7iJQBIpAESgCReBeIHBP6ixhuyewt9IiUASKQBEoAkWgCJwfgRK282PVnEWgCBSBbSDQVhaBInDtEChhu3ZD2g4VgSJQBIpAESgC1w2BErbrNqLb6E9bWQSKQBEoAkWgCFwAgRK2C4DVrEWgCBSBIlAEisAhIXBz2lLCdnPGuj0tAkWgCBSBIlAENopACdtGB67NLgJFYBsItJVFoAgUgctAoITtMlBsGUWgCBSBIlAEikAR2CMCJWx7BHcbRbeVRaAIFIEiUASKwKEjUMJ26CPU9hWBIlAEikAR2AICbeNeEShh2yu8N6rwh0pvnyBSnQoIPYpAEbg0BB71VkkPmvMjRnqcDwE2+bGT9SEiPa4BAnWu2x/EB0kXHify/JGXiDxF5NEizxq5iuMRUskHR74+8roR7cnpyo5HTk1PGXnOyINHLvPgIGD7xCn0JHn8PNv6waDTm6dPR+hOTns7OJEnSenPEtmn86ULxuZJU4/6HmW32+Xy2KHvJhoj3yMdy9GEy0bgPinw8SJ07mFyPum4Xx68SOT1It4xhm+e62eMXLXvUt9jpF56MoResQv68nB5dhWH7+fJUhH7/rA5n3bA642T4XsinxRxn1OPrSJACbfa9rZ7t3uwgPCSkfeJ+IAfM+c3jHxC5MUiV3FwvNrw/qkMcfuvnM86ONLLInbPlco+K/KjkTETz+WlHMjoq6ekT4/8QOTDI28XefvIu0U+J/Itka1/R4+VPnxQ5Fsj+9YbY/9+qef7InQ2p70ciOfrp+TPiBg7da6RgxfI82+I/FjEuD5Hzj32i4Bvn934ylQD85yOHQjaUyfV+Hxdzv8f+YvIj0deOYI85XRlh8nbS6e2D4j8VORLI28TeYfIu0TY4GfO+X6RfR6Pm8LZoe/N2UQ1p6MDeUPIZlv0R3niu4bXy+baOaceW0VgHtyt9uH6tPviPXmGvPIGkS+JfEzk82+JWd9P5PoqDhGof0lFjGlO5zrM+hiYc2U+I9M35/lnRxj0nC71+OuUhvwyjsgbfBlngrAxhvqxz0hRmrD34/dSwztH9DenvR6/ntI/MfJnkX0ef5zCjc9X5fxbERHYJ895PkTXnjAJnLGIHOf7bbnvsV8E/jvFf2Pk+yMwf/Scl8dDJsHkAen4k1w7fOM/kwsT1RfK+SqP/0xlnxv5kIhv/idz1vZ3yvnjI6KFH5kzkpnT3o7fTMls0p/mPB+i48+dBGQ4pzuOv8zd30dMzHLqsVUESti2OnK7nZmc2acx/NWpG4zbr+T+NyJXcTAQDOn/nbMyxOd5k5eTzOlSjn++lFLWC/nfJIsa6uO/55qz+Z+c/y0icvOdOdsnktOmD/3Sv6vohHpgeBV1GTvRM/pmKc13M+q1pPW3ueHMjK+xzm2PK0AA1l+Teh4+8jKR5SFCivz8Qh7QzZyOjn/Nv8jSK+Z8n8gDc9AFy5sXITKjLUN/3f9+GvFdERHj58t538d/pAL15nR08AFPlysT9duY5H4+4C3fnNbrjSHQAdzYgE3NNcu0BPhsSZsjBz5m+xX+JulXcZykQ8LzjCpheM1ARTREBM0GLY3ZO2S2Otr5NLl4zcgrRdZm3fIy7vaxWAplcJN1x9k6E3s8LJcw+JYPpK0J/ERV1p6dlaYvr5FM9q2IFChHXc+TNIZTxI3x1pYk7fRbRMBytVmwe+mMq36K/lhOUQ4SYVnuoWWIKNfeRJGg3N4+vIsovmpS7PPRn1weHcq0/AFHujEvnRxlWPkHhmR+ZGw5AWOCZA+8Rx7Pnz03bxnRRnjk8ujQj1fLlfbR01zePpb16It9OfYqvWhywXD5TpJvH3M9txNPuRCN4ORfMHnmPWqwRdZEiPPojoOuaYsx840hfDJoK0w4ZnpgrGFjvE1e5DlNjL3xeOFkoqP67ftQbpKODuXaD/pMuRvj6rnvxVgYb/qkLLpOf+iMNtI5xFRffYMpYqcOe1zpgneM2dPmgfH0Drz1VZ+TfMfhm3ydpPhm5c3l6kEX1K3M1QwriZY3fzvpviXv5/L2ASPY/sHtlAdc/HIu9WX0L7d3dZhkqgf+Fy1gqcOW29nef1gUJP3Fk+Z7nHUaVibclu2tlNAx3z8CKR3eMLHNxdga8xRzdMx10wvjRy/YVHZijYB6R96jAvrPNhGgENtseVvNOIiiMej2b1kqsE+HMxFhE8JfQ0l+Dn1NOJAhjOF59IORYfjM4EZ9DMfH5kY6Z2jJFpnh1F4u6d6xpPieueZcctq9Rf6xjMvgPVWuvzzCETEyhPNSDsP2h3n2eRHLeDndcVhGgcVPJxVxzenYgQzZc2JJhhM8luGEBJhYAtYuhlW7LKFZcrPXzZ6cT8m7lizsnXr3XFva+factdUyoH0v2oU4cG4vlWdfGIGXvSna/2a5/7LIe0U4bk7TPitGP0k79dqT4j2G/+WT+LUR14w3PRBZQkY+M+mcdU4XOjhn+4s+NG+JJmqXMeFAkrRDFIyBPX0mB2+SREtCnCzy9sO556zo43fkGknIaRy3z3QMsUTCfyepIrCWnk8aF30RdUESk/1cB53SHvo32q+dojt0iTObC/Lsu5Pw2pG/ihjHj8pZfmMGl6/OPX00bsiPvU1fkDS6n9OJB9yMjx/pfFpyvWnE2NoHSlfgbYkN5u+YZ+8RkY4UfkSuLREi6l+Ra+30PWmH9sEbbp+cZ/SB08/lzndmH5iy3zUJdMS+TO1969wjd0iTvVkcfpJ2dEnfLAGyJe+dREuA9CKXxw6kwnI3kn7s4QkJbIb9pwil73vOpu2W8taW6S3hw0e753cuem3cjT8dncnUecqBj3eMp7FDan2zxnK8TxdgzGaxt3TKNTtiTBBFqyMmBcZZWTBmU4wLW80+GOsvHoUuzsb8jZLGLsGDbikvSXccInInjd0dGXtzuAgwlofburbsNAQYG0aYQWUIzJztrWLsGYqT3hVV4GRPk7fNyz56RimXqwdjwjlwxAgicjAymv2qx1KByAbiYY8bp/lxyWQZikHipH8o9w5LHQgU8onQWNpFVugox6S90ryPACB3iIJ3hzBe2uzHAGaponvj2Xy2nGEviPYw/POz067NhuGNXCJcI+8/5oJzs59OZMm+Fvu09B25QkT00/IpJw0zxlo7GHSkapBUzzlTz5ED/XXPGSKKcGeclc+Q2/z8galffk5X/Qw/rBBX2P95nl/0QAgRE45FFJEj0BekUiSGkzDOxtEPL4wdMjHqsVwMX7jov/1k49l8Nl5IO4KJ0MLQ+CP6c75xbcz9YIHTHmlnnX0rSJ53EU/5TUzoCx11Pwvc4a39fohhj6QIB303ZvDW59/NSwiQMTMBQeI5bt9iHq0eIkbvmye/FLE0DBd7o/TnVZImAg1bmCNiHDaSaTzomWihfXl03DeD8Hgf+cnru1/LP+wAIpLLo8O3ox++I6Rbf7RZn5AiePvm6Q1C7CVkVcSTriOX9Ix+IdSeL8U37deI9G757LR7hEW7RBx96yMvUqq/xm6kjbMxQMJ9ByNteTaGxuEsEeFDSNkKNouOL8ta3suDpJs0+D4+NRlEC+nF0Ft57CvWVmfjxB58WPIaTyTVd2mc/GDLM9cmdOM7Mik3tmyENubVY4ftIIi4+vkDdtv9MqPv8YmSqF42JJc9tobA/IFsre17be+BF84YcJqMA8f9WmmvqIyPW3jdh5mk1YOTMVteE1EnwqF8dN5Wfk6rh9kc4yNaxfjPxIfBERVgyEQFGF8Ea7WgW4lmp8iQJU/Gk0Nl2PTVn+5AwCyFMOJe4UiWs07GChYI7Joj9h5h/DguxlYERdp5BHmCMwPLuM7vcCxmsRwm5/VFeWimDBez55/LPUdsKRdmZtxJOlrOZdQ5bBvlpSnnn3LBqTPaIpWcM+dj2U0Ea+Aj6sbZIaBm/MrgeLUV9gw0Z5DiLnRwRggOguFF/UMAOXmRKqRT+5AOwiGpE3FHTOmPqInxsITpTx8oZynGQtsRE5EhvzRWxqh3mZ+D1zZ1LJ+ddE+H6P3PJoMlWpEG3wg9hXWS7zj0G9FFPvxARlREH0TYZDQe3uNwEU1piBR8fBfySjtJkCtiXDhc4yRKC1vplluNK0JM35Un+oVMwQjJU9f4Pr1jfEZ97unUuPdMO9UxIlb0i+7DxXN66j26ieyIGklTp7aIvCFJJxE2ZZuAIbmj3tPOxkTUkH4YFxE6kafxDl2nV+N+edb3edI0P1e2ZWUE01LhaSIyjsAgp3TvNBI46oCXsTMOCLszjOjwyKNtSDZdEwX1XJv5XDj6ntkfZA5uQ5+VTbdGOc7GTrrruxU2mi6x775dY3y3ZfW9e4QA5blHVbfaBwIBxswyEgetmL/LPzZWW6YRyeJQkrR6iGggdWeJiA1js1pIEhEiM30fvsgPI5nko8MzEQfEiaE387Rsd/Tw1j90D6kTEZLEuHJcjCfjz6iNMpEUBHU2XIjM0rAx8NrDyXA8yj1JYEROen5SujZYbmVg1bfMxwHPaaIHolPIJAeP6Hlv9E1exlh/nN0TfVs6Xeneg4W2IzUiBAQBF3FDaJCNH0zmgT0SnNsLHQihF+Y2cejGglOD79xm7UW+vINQmuUj2qJrCJj0NfGeJWM6oh+IOadCP9fyS/OO83kFZjAXmTN5sI8Q6eR018qwB8gSp4gXUocE66ty1vJLM1b6j9TBSNpZgjSNPMr2PagHDuRH8pBTR+Ztarc069uAEZKA0CXLji44z6K8+d44ws0YSveOa2nuiTzjPeMLM+0gIsTIiSiovEvxLgyW6Wv36rAHT3RW9Mg2AnvJLP2P/L6z03Bkm06rD+k2wTuP0E/jzH4iVaMN5zlrpwgkImsywP54z1i6RpDhR0QpLXmKaprQ2NLADsLBBNR37V3jIt01gZfzSQJ7MvIhq+qf80tD/H1r+ir//LzXG0BgVooNNLdNvIWAPWBmc4zqraSjfU0+SM6FjPTlWWTHstBZgvQghsv3xz3H8U25MfNnqAZ5TNLOfjX7QuyrsbQimjX2tgxDQfc4dETOUpMlGUur8nKkDJ2ybNi1xGuJSuQIoZOOeHJYsFCWNEuLjKC67IGRdpIwsLAY756Ub04feTkDbeWg5+euOULnIWbZMDCTRvJE4DhCzzlg52FoBzbS1mTkQxg9N0sXuSMckygWMoJwIL+W50T6REu0nQNExEc5yhgiTZ5xL+oBo1kHRDy1XRQFETNunJJ3vCuaRw9EgURn7EWzxC3iqHzLvqIqrr3jTGf9fSs6K7JLr5EC/ZBnKd5R2ue18AAACtJJREFUj/qWz866R9g4S8u6yJKIw9o7iJoJERJpmY/ujbFGhtfesbyKDHLMMNJOeMFwLb80jtmZuPZNwcNYGlNiAkDnTYAQN+RfxEZ7/FDAu3ROfcS9b2K2DdLOI0P/tEWfRYnmthhTbVkrC4HS/1lf1vJpoyizqLMJBlL688kIM0vjysntztjA1PWaIMbGcu2ZfnhGh0UiTxOY0mM6ISKOvK2VuUyb9c8Y6ZfxZtPkNXEWdfV9GMch2u07YF8teSNr7JVotKivd+E/l+8d365nRF3z2TXxjmciisZO2hD7IbUJKTVppDPjWc8bQcAAb6SpbeaEgL0WHJ/lF7Mysylpwvpme5zplP2OS9EvROosEUJniO54eXHjo0es1M9JjMeiM5wKo8GxqJMD8JzzNotFthg3xkk5njkzTGba9gMp1x/YFHWwoV/kBhFjyBEPkSP5GTRGWn57NTg+y3OIy3AAyh8CMxEpzlgUaqQvz74PeTk/hlB/1KVMbR35PZMHiXTmpGfHpm1E+7SfYxN5Q0q8oz/O4z1OW15nuKrTc85Qezg5e6iMNzIr7a3SGARU9AvJtbwEewQC/torDzKMyCX7rWO3GxiO8tWJrHCilqeVjwwiU4in8uyJQsqRBs+NmaiJMhSsv5wVJ+SZPoqmGhP98QwG0kXTLANqhzrpLwKnnKXQAZFbDn/5bL4f5VtqUz5s6bNJBmJgv48xku5sDI3vaD+9lKZ9NulrN0xFk0c9dNMkwphbZpLPhEN02DfgR0FIKDzHO9ql/3RI/12rx3M/4rBMCXtjj/xaRtc+37co9H2TUT8s11pKzu1OhF0Z2qo80UPjRR/0R5o20iFj5Vrd8FYPXfMtuvfcve0MSC2CKo/+WLqjR+pcCntkiRxZWD6b70V62B77UIny2BB7IJWhHvnpALy1xf0s2qpNdHlOv5trRIltMlZ09qQy4As3OiKPa22AmT6YiMFUpBBOSJm9hJbTxzfimfH0jjQ2TJTQmBtPOChb5JxeqoN90kb3CCE8jKcytMU9Im0S6bk2sQFL4kmXjKf61FHZIAI+/g02+8Y3maMUTWKUOQTRKfuqzBDtBTvN8Fw2eMOxMWijbDM5Rsgv4JAEjlrEx3MzTY7J3iDRJ3+LSYje/hHGyZKB2adImUgLHUVQvGNJQV9tokZ2pCEnjB8CIQohcmh2TUR6OGf1zoIA2qRrhs+5zs/ma0bOrzuRLNEZ+13ss0MC5nycnM3ijKqykRp/pkMexMCmbm1jyI0NMomg2DgukoNQWC41lt6zPIIUvUIKQIIszzHMylcPxyuPyISIi2UzRltdsETULMHab8dx2OeYonYic3QE0XA/BKlSt3dNAiy5cSCIHSJiiRJBssyJyHvPjwM4JD/E4NS1U+QN0UIkOTF91gdjLxKr3lEusmFfD+HsEBJ704yvsVO+epaCgPolniXf5bP5Xnl0CqESkaNXyIAx9ytefYW7yIpoDsJgXJBSeka3vE9PRTKNqb7YtznqQZyMBx33DA6iqMaYczR5GsRsvKNd3tFHOCvPeHou2kPHEC2/uKXnlkJ9SyYhvjH9oJOWKD3znjrhiwwQBBFGxkYd9IP+IBTqJiLb6vPDHm23RGf8kAMTIaRBHhMCGGmL6Iw+q3Mp9FU0V+R5+Wy+R2C0XfTVt+KZCZzv1/Kib0ma6BgCCi/3s9Bp3+1Zdc3vrF0jPfRUJGxeGl7LC1PYilRpK7Lku2CL5ae79gOaqCJObJcxgJ0Iuwmi/XrGDoYIKV1jE3ynCLJ3lGW5m131LZgwsX/Gl96KRiPF9ME3zobCj17rj/L9Qpg9UNYQukMvx33PG0SAMzz0Zrd9xxFANjhP5AFx42zsLxFiZ/SOv7G/lDUjYBmMseEEbEb3q0kkTiu0zy8HOQDPOXDpjKClVfn9SQyRCoQPETUr5DT1VblIA+NlFomkMIScCxLDacjjHhlg6JQ/C+PsPRiK+M3P5mtOFVnSLuVZKlM2hzPn45wZXPVxfv7EhTGShzHl+Dl1JAAZ0H9OU7THmI3yOU/vjf5w3vac6BfDrXzkBxZIhnJhKw3543y0TRuRD/uDOGskSluUjSAgh+6HMP7ImT5ankEAOIxfTAakDHlQj7bO4y1Coq3qQo7tn8srO/splYeo2p+kPOUil3TDMhCc9NtzY8bRGHf39GLpcJRLODq/XlWO+5ME4fCLO31Sl3Gkg4gi8uk9usHRIs3yGRN6QSeQL21CLEyCYOA5DL1LlCVaBGfji6DRb89ECumXX6e6lka0C+kynvQJFsbTMyLaQ3+QAeMqYkz//fqb06f39AfmYyw4euNsDPwC1HN9VwfsxzX9QSDoqvGAi7p8a8gIDIwJYuYbMZ6eGzd6Y1zpmHYuBWFDZkwKls/me8QCIREVHOn6gYDDQjnS6ShSLSqFbEgjIpQmESLMA2vpdyNwRYYs+571PnIJX2QJTrA0IaED3vXdIWZ0mR7TIeXrL92g03RQvxE7mLqX32TJ/01FOcR357tiC42VyQJdMREywTWJMHbG0D5i79BL9RgnkzJps/D1vuk5rdcbQ8AgbqzJbW4QQCQYA0bEx89IMgR5dOWHNpjZWbYZlTOkDD6ni1gwyOOZs3vtFQlyP8S9ZQHvStNH51mUZ3Y9p23hGrnUbn3X3rW+Sb+IcJ7wQBhHubCDPxKgzpGuXFECRt8M3P15RDuNlXFeyz/GbOkMtMEyjbP3lOO8FO1ThvfhI/qwzHOv7jld+Goj0UbkwZKTaB2xVEpnZ1KmvYgFsmFsiLTzirrUbQxde29cw8ckYKR7RrTNO57RAWPv+1vmk/ciQseUq6yLvHdSXmO9xEpe+gFHz93TOWTKMrMlQGnEqgLMbWdwf0E5lp1+wuvYg7tIgDmyZWIw4w47GBojxeqrfqpXurP0WaTBQD5jqmzXc57ltXro6zLdvQirvrqubBSBEraNDtwBNduGXks6Znv2nBxQ09qUBQIcnf1fSP7iUW/PiQAiZlkKceIARbHWXmVbkTvEYnbea3mbdhwBpEYEF3b25MmBdFjiNUG1jC6tcjoC9suKFMPOt3967j49aAQYlYNuYBt38AhYTrNMIMJmuYVhOPhGH2oD99wuy02WWEVN9lzVtS0ekbBUaX+YJVJLUGudFSHxx29FDdeeN+1sBESLLEOKaCK/9nCKNM5Lz2eXcnNz+FGD5Xf7Au35RXRvLhrXoOclbNdgEO9xF8yAkTb7Mew5W1vuuMdNbPVFoAhsFAGkzT5Cdsa+Nnu2Tlqe32gX99ZsexDtbWabYbe3ilrw1SBwQcJ2NY1qLUWgCBSBIlAEikARKAIPQKCE7QFY9KoIFIEiUASuCoHWUwSKwIUQKGG7EFzNXASKQBEoAkWgCBSBq0eghO3qMW+N20CgrSwCRaAIFIEicDAIlLAdzFC0IUWgCBSBIlAEisD1Q+ByelTCdjk4tpQiUASKQBEoAkWgCOwNgRK2vUHbgotAESgC20CgrSwCReDwEShhO/wxaguLQBEoAkWgCBSBG45ACdsNV4BtdL+tLAJFoAgUgSJwsxEoYbvZ49/eF4EiUASKQBG4OQhsuKclbBsevDa9CBSBIlAEikARuBkIlLDdjHFuL4tAEdgGAm1lESgCRWAVgRK2VViaWASKQBEoAkWgCBSBw0GghO1wxmIbLWkri0ARKAJFoAgUgStH4P4AAAD//5L6feAAAAAGSURBVAMAS3k6qrL9cwIAAAAASUVORK5CYII=>

[image4]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAYCAYAAACIhL/AAAADJElEQVR4AeSW2atOURiHzzFPZR4vpCihlOQCSSIyJvwBImNCSZEyxY0LSYmQGyUpMoaElCnTBWVKcqGUeUjI+Dyfvba999nfcPqOUk6/Z7/vetfaa7/f2mu9+zSq+cf//ssEO/BSWkGlasbAzpCrcivYgrtGwiToAeXkmIMMagqVqjED90M/qKNiCfZl5FE4DiY4BHsOTkNXyFNzgj5oOfYdZNWEwBLIru4nYgvAe+vMnU2wloEr4QLsgjGwHtbAIGgJV6ENZLWRwB24AUGu5EIam+ExbIHWkNVDAofAcZg/yibogNV0T4FjkJS/1OR7EVwGSbWlMQe2QlLO7zY5SfASlNJ2OqfCAIjlBKFh51IaG+Aa5MnV+UnHZEhqJo0HEZhYX/D80Wew76GUntN5HuZCrJBgOyLb4C34GjC5+k7UBPtgk5pB4zJUqytMMBpihQRnEfEE7sN+hGLqSYf3mChuQZ5C96erWwhUcbnOvb7ijtiCfJjOBC9wCkppeNR5L7Iaf5gb31dkuxrCHN3DJCZooRwRBfwFkZtrwt5LHiC3h4PfeKkAK0WxYW4x+1Ir6A3WqB/0vIBi6kKHp9satxs/yB+o/9lLBfi8YsPCHHGtdAU9aU+4Q9+6hZsra6HF2EL8KjHideRbaiK3pCmVYJgjPvEm5WwXvcBgyNNEgvNhB1jAMbFeRl77yJYzpRIMc8RvMiS4glndQ66Sp5JmrOl4foY2YRdBVh8IPIVs6SGUUtgKvoVUR6LRG9/5HmELCgk+ozUOuoH1bDHWr8VZ7DwYD35FkuWFUCzHDYtbaec2TYv4NKyvzk/lXXxLGyalobR8vucBt6YmJGjDE+wr9tvpat4nOBvGQtgCuLk6QdRK4GHDTWkgLf/5cH+Ji9Cf2B7IahSBnRArmaDBr1xuwl7woR4e3LLyQ+/BcTuUHVxkgMn5T8iRZH82wWRffXxfvfvYE57dw5XMU8ugcL9z0fythkrQ2Szeh3HWQn3lnr/FTQcgpYZM0In9T+gbTieoVH6J3Jer8m5o6AR9xjouoTbilpWfNytEfHKTd/yNBJPzV+3/AgAA//981Xc1AAAABklEQVQDAKLifzECH9stAAAAAElFTkSuQmCC>

[image5]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEYAAAAYCAYAAABHqosDAAAFh0lEQVR4AeyYZ6geRRSGN/besSsqigVUVFTsBnsXVPSHXSzYEOx/7IpdEURRsSB2LNhISO+dJCQhCQkkJIGE9F5IfZ7NzmZnv918yb0JfIFc3nfPzJnZ+WbPnDlz5u6QbP+rtMB2w1SaJUm2hmEO4Lf2gK2OQ5jgzrASzQyzG29dAq+Hh8NmsM/vdCr+4C7UD4KthhOZ0C9wR9iAOsP40t/0/hdqmLOR3WFnqKURDdgVzc/wWbgAHgnnwhVwCGw19GZCzusTZAPKhulAjxdhL/glvBy+Bl+GZ8Dd4UC4FyzjTRSj4FAopvHQiIOQrYp3mdhV8AoYoWyYD2l9Cd4I/4FFLKOi0Y5BPg2L2JfKg7Bs/ZXopsJWxWom5pxfR0YoGuZmWp6Cb8DBsAp6w1oaboBF3EtlfEZEhDVRrfUqbv9zmZY7ArEewTD7Uf0UzocfwzpoYQ1zfKnDrdT7wyrYv0rvtryJhsfgRdBtjIiwEzUnbL/DKO8Dn4R6J6IBe6LR241vFBMD65kUfP9gZBVmopwAL4M5gmHuR+OJ8iNyCazD0TT4jgaimMIfd/J6U6rYhMf59BkDT4CT4N2wCzQmIVIcx9OYpSFOomxsM2DqgV9QPxAW4bxc/QtRjobGDU/IqylfAyfCk2EVnLuLk7c5mJVrfcBOcGO4IGscm0mFBnWltLz1ZjyCDv7O28j34f9QD3BBvqUcPOd7ysvhffCdjKcie8Cj4BxYhEHUU+YrlMY8jee4b1F/Bu4NyyEAVQrn7nekFR8axjwjWMuB1dcxDFwMzG5D+8/zsQl8jj5O8k9kEf9RcXXDXNwCnmyoU1h2vmdRs4yIoMF/QmNqgUiMe7OS9X9uQ0umEcoynXuUa/lDrpB7WRcNA5VftO4edf86uKuiTmpYpaurbEY/zD7mOMrAxVkhtPelfiwMcGu5heuOf+dkrLiUF5yj71NM4bay0NNHBc21omxdw6icTGfLxYwVVQRzGZM4E7iiG4cP1H2jF2oqLoBN0URQuDiIJMzBD3Wl3VLv0fA8vAt6+iFqoWH60KoRESlu42ncqXvXuS+kTw6NYSVYN6yWuiKvo/II/Bya+CFyzM5K+2eyLPwNvTLoDaCWjRPKwFDvlilMHzomSfIBdbedJ6FbhWotPI3sZ4IaOmnci6l4sGj07yiXYTiIdouTttMLPNxneoWnDNUct1Ay2pslPk65jEUo3PNOiGID3Gp6QzCOHzqFXk/AADNpveEHFMOg0JPNuo0Znl7e186hwbEQldAANnh9UcrzePidXnHuoDwSluHchxeVvmB9Og+j+qFI8xGPSLNbV+9hdB53Zr1F90Sdw34ewbmCgke7e74jZfMPj2U/zgXwSLV9AG3fwH7QiXsCUUxhgDf/8HTxtHJxjC/jaPUuh2jAaWjMtEcgAzxQ9Gq/R8OZr4U2pY5gghd2jbro3w4O4FZ6lBYn7wQeoHwljF6iXoYniqdJcTX1Clfa/athvEp4KfVdJ+/29JTzKHZVPa28Qth+Cg9d3qCpx+lt3vT9AGPBR7RXwaPZbwhxzD7+lr9t2v8QCj0RkcP+Hhx/5BoKwWMopnBiurIBz481KKcNTR4OakB22zXpGjW7ki7A0kibJN7oZ6DrCp0TIvGDvKpoMF1fXZkaLYoVWQdzJD22Kgs389YrHT/rnkQekyvbUHCLGac8sXTNNgwRvfIbNQOlp5GBkWrSgYce5H2uzmPoslkwHdBbG65BZY/ZrFFLnY0Jf6F7BbYXetLpDOKW/hXpdUHeSdnrw2fI9sIt+jWD3A79PcQGbEnDOKo381UUoiySelvghdaYYYzTU/zf0D0MZI6CaDfMsr2SRKdRGHVLG8ZxX+XRsALoWg2egsbRynltDcNU/tC2plwHAAD//4iXlq8AAAAGSURBVAMAvHr3MbKDAIoAAAAASUVORK5CYII=>

[image6]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFAAAAAYCAYAAABtGnqsAAAFfklEQVR4AeyYdagtVRTGty2KndiC3S12i4H67EJFxUKxE/sPC7FQUVFREVsMVETFTmxExA4UG7vlxe937uy5M3P2zIl7HtwXh++btWPNnpk1a6+15kwbpv5GZIGpBhyR+UKYGAacn3uaAU4uWKTpQQZtwJW42C2wG6yN0nJwtON0bnAvmETVgBug9RH8Gn4LlZ8hP4bKV5Enw5lgFfMwcDM8HP4Pm6CRT0DhWXg0HM04nps7Dm4I21A14ItoLA2fgwvAXeGScCm4Arweng9vg1Vcx8Bd8AvYhG2Z3B8+Dt3ucyJHM/7m5k6Bt8IZYQlVA8bJdWj8CvU4RAsupAE/oRcNS7MFDbwVrRtgJ+yBgka+G7kDvAiOdjzDDf4ED4YlpAy4KBp63fPIsbAI9RfOBmxnzXAkjcfgz7ATNkHhbfgHfBj+CycF3MlNGp4QwygaIY5unDWMT1kzFz78rPRegXoiooXdOL4M6zANE3rp6khfjrHVUDEv/X4xOyduD9eE/cIM6y6YO1tgMeSOMPZp5niJ1srQsIMYQsqAmw5NhaczGYWx6kI6X8F9YcSCNLyR15B18KJXMnkjFGZg+7vb6YP7cI4v0ZdigH+Q/pewl/LpNPTPhlYC7yFNapch14Ovw+lhEW9knVIyqTPgOJQN9gZP0/g19F3UC61K+1MYoSfZ/t5DDb9j3PXM0jTDnhy2ga6L6AmHoH0T3BleAfeDeobhplP2R7WFXTguDl3rYqS76VTkAXALaAKtvox/GP8NLgRzVA2oJ/lWjVHvovU+fAca8L3JA2kbTBE59Ew73cQ/t7A3YUnkOb3S+HwJJ90OP4ARhpUXYqcLuQo6Z0JheNEJHqVjXHb9MbRNmogSfEbLtXywasC4fR9C4wHo1rDtdk4tiEqIqd03ZL+Jeq8vZ3yTUsPcQczNBu+DEcvTMERYetHsCBXO4fADFJZn89Ew0yKCpdgTNhL0GWcpjlcNuFk2GRfLuo0ieuQcjVqhZegV0XkT9gsThuGlmODiPRfHelk/nt/NM7vb3EH5+lUDbs6MntaUUVEp4cesN1cm64RvWm9tMqDbQ726Nf5j4htYfAjrT7N6cUt7HROV2xP1NhjrzspGt0SaGGNc15svZSwFDRg9tzVfNKDZaAlGzaa91GZe2AczdnJ6LdbIZmI2y7olYaIy5i5bGh3uGKcM8D6Ioydy2AlWvcevIj8C2gpfdP0Mdd6kprHiC2AqaPDzaBi6ECVY/87MyFswhwY0bfv9a52j963FrF8K1nY0O+JPNPTY9ZFN0IC/oGBiQiThOt7DRsnZEMzil4cQ/OT0U9ASim6oGlAvN7TEmjYUfjqH53sffpL6ZaTnWxHcj55zqXDg8+n5vmDUhqABvWmzkMWjAdKMZoq/d0ilq+MjaMVYQjMJX8xTzBjDEElY3+k10yVnQ7BUOYk5Y+nWyM+hcF1lpDWmyaVay8V5vU5PG8OA974M0hrQ6/uS6LbB53POF5BPasC8M4LGtZyr0ddFFqEn6R0+8GpM3AE7wZikd1X19GBjk1swzvlNbrllHRfHonSdumzqS/QfJneP+vY/pPEXTMEEqXGvrk4OyoC/s/C50OoekeMwWpYueotbxi3CUC38E8MdkKoTL+AsC1/jkB56Bn2NegSyCh/4UAbvgYPAUSzidtfINIcxKAO64lUczMhuQZot6Jl6oJ5psHcLtiZqDn4nH1MzZ4HrHxwW0U+iY+1mYZ4qoN2S/m9pYYzqiOBLdfsem1plkAZ0fb3BeBq3mQ/nJ5f/6JqY1GmiRawvIaXjtt6OCf9AsODX0FYADLXBSsIs3DbRx4Avfm/OS34mDtqAXsRvylKg5eKTMvwvoFT7FR9m0AYsrj1FtCcAAAD//8MuxW4AAAAGSURBVAMA2Rz2Mes6gWgAAAAASUVORK5CYII=>
