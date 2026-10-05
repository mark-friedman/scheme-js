import { schemeEvalAsync } from './scheme_entry.js';

/**
 * The name an inline script is read under: the page's file and the script's
 * place among the page's inline scripts, `index.html#scheme-2`. A debugger
 * shows it, and the source maps of what is compiled from it name it.
 * @param {string} pageUrl - The page's URL.
 * @param {number} place - The script's place among the inline ones, from one.
 * @returns {string} The name.
 */
function inlineScriptName(pageUrl, place) {
    const path = pageUrl.split(/[?#]/)[0];
    const page = path.slice(path.lastIndexOf('/') + 1) || 'index.html';
    return `${page}#scheme-${place}`;
}

/**
 * Runs Scheme scripts in order, each read under a name: a script with a `src`
 * under its URL, which a debugger can fetch it from, and an inline one under
 * `inlineScriptName`, its text kept since nothing could fetch it. Each is a
 * program: one that begins with import declarations sees only them, and what
 * it defines is its own; one with none shares the page's environment with
 * the others like it.
 * @param {Iterable<{src: string, textContent: string}>} [scripts] - The
 *   scripts; the page's `<script type="text/scheme">` elements by default.
 * @param {string} [pageUrl] - The page's URL; the document's by default.
 * @returns {Promise<void>}
 */
export async function runScripts(
    scripts = document.querySelectorAll('script[type="text/scheme"]'),
    pageUrl = document.URL) {
    let inlinePlace = 0;
    for (const script of scripts) {
        try {
            if (script.src) {
                const response = await fetch(script.src);
                if (!response.ok) {
                    throw new Error(`Failed to load Scheme script: ${script.src}`);
                }
                const code = await response.text();
                await schemeEvalAsync(code, { filename: script.src, program: true });
            } else {
                inlinePlace++;
                await schemeEvalAsync(script.textContent,
                    { filename: inlineScriptName(pageUrl, inlinePlace), inline: true, program: true });
            }
        } catch (err) {
            console.error('Error executing Scheme script:', err);
        }
    }
}

// On a page, the page's scripts run once it has loaded; anywhere else -- a
// test importing this module -- nothing runs until asked.
if (typeof document !== 'undefined') {
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', () => runScripts());
    } else {
        runScripts();
    }
}
