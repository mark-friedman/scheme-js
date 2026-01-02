export default [
  {
    input: 'src/scheme_entry.js',
    output: {
      file: 'dist/scheme.js',
      format: 'es'
    },
    external: ['fs', 'node:fs', 'path', 'node:path']
  },
  {
    input: 'src/html_adapter.js',
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
  }
];
