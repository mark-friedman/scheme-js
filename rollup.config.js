export default [
  {
    input: 'src/packaging/scheme_entry.js',
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
      format: 'es'
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
      format: 'es'
    }
  },
  {
    input: 'src/packaging/scheme_repl_wc.js',
    external: ['./scheme.js'], // Crucial: treat scheme.js as external
    output: {
      file: 'dist/scheme-repl.js',
      format: 'es'
    }
  }
];
