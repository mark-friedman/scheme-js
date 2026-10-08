/**
 * Leaves the pinned seed out of a bundle: a bundle is built with its prebuilt
 * tables, so the library system's seed never needs it there, and it is the
 * seed libraries over again (src/core/interpreter/library_seed.js).
 */
const withoutPinnedSeed = {
  name: 'without-pinned-seed',
  resolveId(source) {
    return source.endsWith('/pinned_seed.js') ? '\0pinned-seed' : null;
  },
  load(id) {
    return id === '\0pinned-seed' ? 'export default null;' : null;
  }
};

/**
 * What every output's source map says: where each line of the bundle came
 * from, and that every source in it is the system's, which a debugger skips
 * as ignore-listed -- DevTools does so unless its user asks otherwise -- so
 * that a step goes from a program's Scheme to its JavaScript and back without
 * stopping in the system between (`x_google_ignoreList`).
 */
const SYSTEM_MAP = { sourcemap: true, sourcemapIgnoreList: () => true };

export default [
  {
    input: 'src/packaging/scheme_entry.js',
    plugins: [withoutPinnedSeed],
    // The compiler's chunk imports what it shares with the bundle from
    // `scheme.js` itself, rather than both importing a third, shared chunk.
    preserveEntrySignatures: 'allow-extension',
    // A directory rather than a file, because the bundle is split: the
    // compiler, reached only through `loadCompiler`'s dynamic import, becomes
    // `dist/scheme_compiler.js`, fetched by the pages that ask for it.
    output: {
      dir: 'dist',
      entryFileNames: 'scheme.js',
      chunkFileNames: '[name].js',
      format: 'es',
      ...SYSTEM_MAP
    },
    external: ['fs', 'node:fs', 'path', 'node:path']
  },
  {
    input: 'src/packaging/html_adapter.js',
    plugins: [
      {
        name: 'rewrite-import',
        resolveId(source) {
          if (source === './scheme_entry.js') {
            return { id: './scheme.js', external: true };
          }
          return null;
        }
      }
    ],
    output: {
      file: 'dist/scheme-html.js',
      format: 'es',
      ...SYSTEM_MAP
    }
  },
  {
    input: 'src/packaging/scheme_repl_wc.js',
    external: ['./scheme.js'], // Crucial: treat scheme.js as external
    output: {
      file: 'dist/scheme-repl.js',
      format: 'es',
      ...SYSTEM_MAP
    }
  }
];
