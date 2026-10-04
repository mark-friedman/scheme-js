/**
 * @fileoverview The text of Scheme code read under a name nothing could fetch
 * it by: a page's inline script.
 *
 * A source map names the file each line of compiled code came from, and a
 * debugger fetches the file to show it. A page's script with a `src` can be
 * fetched by its URL; an inline one cannot, so its text is kept here, under
 * the name it was read under, and written into the source maps of what is
 * compiled from it (`sourcemap.scm`). It is host input, the page's own text as
 * the host read it, which the page's start-up and the compiler, loaded later
 * and apart from it, both reach here.
 */

/**
 * Text by the name it was read under.
 * @type {Map<string, string>}
 */
const texts = new Map();

/**
 * Keeps the text code was read from, under the name it was read under.
 * @param {string} name - The name.
 * @param {string} text - The text.
 */
export function rememberSourceText(name, text) {
    texts.set(name, text);
}

/**
 * The text code read under a name was read from, if it was kept.
 * @param {string} name - The name.
 * @returns {string|undefined} The text.
 */
export function sourceText(name) {
    return texts.get(name);
}
