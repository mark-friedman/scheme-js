import { schemeEvalAsync, interpreter, env } from './scheme_entry.js';
import { parse } from '../core/interpreter/reader.js';
import { analyze } from '../core/interpreter/analyzer.js';
import { list } from '../core/interpreter/cons.js';
import { intern } from '../core/interpreter/symbol.js';
import { SchemeSourceRegistry } from '../debug/devtools/source_registry.js';
import { DevToolsDebugIntegration } from '../debug/devtools/devtools_debug.js';

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

    const devtools = new DevToolsDebugIntegration(sourceRegistry);
    devtools.enable();
    devtools.enableTracking(); // Optional, for diagnostics
    interpreter.devtoolsDebug = devtools;
  }

  let inlineIndex = 0;

  for (const script of scripts) {
    try {
      let code, sourceId;

      if (script.src) {
        const response = await fetch(script.src);
        if (!response.ok) {
          throw new Error(`Failed to load Scheme script: ${script.src}`);
        }
        code = await response.text();
        sourceId = `scheme://app${new URL(script.src, location.href).pathname}`;
      } else {
        code = script.textContent;
        sourceId = `scheme://inline/script-${inlineIndex++}.scm`;
      }

      // Parse with sourceId so AST nodes carry the correct filename
      const expressions = parse(code, { filename: sourceId });
      if (expressions.length === 0) continue;

      // Register source for DevTools probe generation
      if (sourceRegistry) {
        const origin = script.src ? 'external' : 'inline';
        sourceRegistry.register(sourceId, code, origin, expressions);
      }

      let ast;
      if (expressions.length === 1) {
        ast = analyze(expressions[0]);
      } else {
        ast = analyze(list(intern('begin'), ...expressions));
      }

      // Execute using the interpreter directly
      interpreter.run(ast, env);

    } catch (err) {
      console.error('Error executing Scheme script:', err);
    }
  }
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', runScripts);
} else {
  runScripts();
}
