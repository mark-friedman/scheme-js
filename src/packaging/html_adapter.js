import { schemeEvalAsync, interpreter, env } from './scheme_entry.js';
import { parse } from '../core/interpreter/reader.js';
import { analyze } from '../core/interpreter/analyzer.js';
import { list } from '../core/interpreter/cons.js';
import { intern } from '../core/interpreter/symbol.js';
import { SchemeSourceRegistry } from '../debug/devtools/source_registry.js';
import { DevToolsDebugIntegration } from '../debug/devtools/devtools_debug.js';
import { SchemeDebugRuntime } from '../debug/scheme_debug_runtime.js';
import { setFileResolver, getFileResolver } from '../core/interpreter/library_loader.js';

/**
 * Checks whether DevTools debugging is requested via any supported mechanism.
 * @returns {boolean} True if DevTools debug should be enabled
 */
function isDevToolsDebugRequested() {
  // Option 1: Global flag
  if (globalThis.__SCHEME_JS_DEBUG) return true;

  // Option 2: URL parameter
  if (typeof location !== 'undefined') {
    const params = new URLSearchParams(location.search);
    if (params.get('scheme-debug') === 'true') return true;
  }

  // Option 3: Any <script type="text/scheme"> with debug attribute
  if (typeof document !== 'undefined') {
    const scripts = document.querySelectorAll('script[type="text/scheme"][debug]');
    if (scripts.length > 0) return true;
  }

  return false;
}

/**
 * Checks whether library source debugging is requested.
 * Library debugging generates probes for .sld/.scm files loaded via (import),
 * making them visible and debuggable in the DevTools Sources panel.
 * Gated separately since it can be noisy for standard libraries.
 *
 * @returns {boolean} True if library debugging should be enabled
 */
function isLibraryDebugRequested() {
  if (globalThis.__SCHEME_JS_DEBUG_LIBRARIES) return true;

  if (typeof location !== 'undefined') {
    const params = new URLSearchParams(location.search);
    if (params.get('scheme-debug-libraries') === 'true') return true;
  }

  return false;
}

/**
 * Finds and executes all <script type="text/scheme"> tags.
 * Handles both inline code and src attributes sequentially.
 * When DevTools debugging is enabled, registers sources for probe generation.
 * @returns {Promise<void>}
 */
async function runScripts() {
  const scripts = document.querySelectorAll('script[type="text/scheme"]');
  const debugEnabled = isDevToolsDebugRequested();

  /** @type {SchemeSourceRegistry|null} */
  let sourceRegistry = null;

  if (debugEnabled) {
    sourceRegistry = new SchemeSourceRegistry();
    // Store on window for DevTools extension access
    globalThis.__schemeSourceRegistry = sourceRegistry;

    // Create the debug runtime (stack tracer, state inspector, etc.)
    // This must be created BEFORE any scripts run so frame enter/exit hooks fire.
    const debugRuntime = new SchemeDebugRuntime();
    debugRuntime.enable();
    interpreter.setDebugRuntime(debugRuntime);

    const devtools = new DevToolsDebugIntegration(sourceRegistry);
    devtools.enable();
    devtools.enableTracking(); // Optional, for diagnostics
    interpreter.devtoolsDebug = devtools;

    // Wire the DevTools integration into the debug runtime for async stack tagging
    debugRuntime.setDevToolsIntegration(devtools);

    devtools.installSchemeDebugAPI(interpreter); // Expose API to DevTools Extension

    // Restore any pre-loaded breakpoints (set by activate_debug.js from
    // localStorage before page scripts run). This ensures breakpoints are
    // in the BreakpointManager before the first Scheme expression executes.
    if (globalThis.__SCHEME_JS_BREAKPOINTS) {
      for (const { url, line, column } of globalThis.__SCHEME_JS_BREAKPOINTS) {
        try {
          debugRuntime.setBreakpoint(url, line, column || null);
        } catch {
          // Ignore invalid entries
        }
      }
    }

    // If the DevTools panel was connected in the previous session (flag set
    // by activate_debug.js from localStorage), mark panelConnected=true now
    // so handlePause() will block at breakpoints during script execution.
    // The panel will call activate() again after the page loads, which saves
    // the flag for the next reload.
    if (globalThis.__SCHEME_JS_PANELCONNECTED) {
      debugRuntime.panelConnected = true;
    }

    // When library debugging is enabled, wrap the file resolver to register
    // library sources as they are loaded, making them debuggable in DevTools.
    if (isLibraryDebugRequested()) {
      const originalResolver = getFileResolver();
      if (originalResolver) {
        setFileResolver((libraryName) => {
          const source = originalResolver(libraryName);
          // Handle both sync and async resolvers
          if (source instanceof Promise) {
            return source.then(code => {
              const url = devtools.libraryNameToUrl(libraryName);
              devtools.registerLibrarySource(url, code);
              return code;
            });
          }
          const url = devtools.libraryNameToUrl(libraryName);
          devtools.registerLibrarySource(url, source);
          return source;
        });
      }
    }
  }

  // Fetch host HTML for correct source mapping of inline scripts
  let fullHtml = null;
  let documentUrl = null;
  if (debugEnabled && typeof document !== 'undefined' && location.href) {
    try {
      const res = await fetch(location.href);
      if (res.ok) {
        fullHtml = await res.text();
        documentUrl = location.href;
      }
    } catch (e) { /* ignore */ }
  }

  // Pass 1: Fetch, parse, and register ALL sources before executing any.
  // This ensures all sources are visible in the DevTools panel even when
  // execution pauses at a breakpoint in the first script.
  let inlineIndex = 0;
  const prepared = [];
  const allInlineExpressions = [];

  for (const script of scripts) {
    try {
      let code, sourceId;
      let lineOffset = 0;

      if (script.src) {
        const response = await fetch(script.src);
        if (!response.ok) {
          throw new Error(`Failed to load Scheme script: ${script.src}`);
        }
        code = await response.text();
        const url = new URL(script.src, location.href);
        const filename = url.pathname.split('/').pop() || `external-${inlineIndex++}.scm`;
        sourceId = `scheme://scheme-sources/${filename}`;
      } else {
        code = script.textContent;
        if (fullHtml && documentUrl) {
          sourceId = documentUrl;
          const codeIndex = fullHtml.indexOf(code);
          if (codeIndex !== -1) {
            const beforeCode = fullHtml.substring(0, codeIndex);
            lineOffset = (beforeCode.match(/\n/g) || []).length;
          }
        } else {
          sourceId = `scheme://inline-scripts/script-${inlineIndex++}.scm`;
        }
      }

      // Parse with sourceId so AST nodes carry the correct filename.
      // wrapLiterals wraps top-level primitive values for breakpoint source tracking.
      const expressions = parse(code, { filename: sourceId, wrapLiterals: true, lineOffset });
      if (expressions.length === 0) continue;

      // Register source for DevTools probe generation
      if (sourceRegistry) {
        if (!script.src && fullHtml && documentUrl) {
          // Accumulate inline expressions to register the whole HTML file later
          allInlineExpressions.push(...expressions);
        } else {
          const origin = script.src ? 'external' : 'inline';
          sourceRegistry.register(sourceId, code, origin, expressions);
        }
      }

      let ast;
      if (expressions.length === 1) {
        ast = analyze(expressions[0]);
      } else {
        ast = analyze(list(intern('begin'), ...expressions));
      }

      prepared.push({ ast, sourceId });

    } catch (err) {
      console.error('Error loading Scheme script:', err);
    }
  }

  // Register the full HTML file integrating all inline scripts
  if (debugEnabled && sourceRegistry && fullHtml && documentUrl && allInlineExpressions.length > 0) {
    sourceRegistry.register(documentUrl, fullHtml, 'inline', allInlineExpressions);
  }

  // Pass 2: Execute all scripts sequentially.
  // When debug is active, use the async path so the cooperative pause
  // mechanism (PauseController.waitForResume) can block execution at
  // breakpoints and step boundaries without freezing the browser.
  for (const { ast, sourceId } of prepared) {
    try {
      if (debugEnabled) {
        await interpreter.runAsync(ast, env);
      } else {
        interpreter.run(ast, env);
      }
    } catch (err) {
      console.error(`Error executing Scheme script ${sourceId}:`, err);
    }
  }
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', runScripts);
} else {
  runScripts();
}
