export default [
  {
    input: 'src/packaging/scheme_entry.js',
    output: {
      file: 'dist/scheme.js',
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
