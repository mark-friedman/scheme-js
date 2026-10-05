/**
 * @fileoverview R7RS-small required identifiers, by library.
 *
 * Transcribed from Appendix A of the R7RS-small standard (see
 * `docs/r7rs-small errata-corrected.pdf`). This is the *required* surface --
 * what the standard says a conforming implementation must provide -- as
 * distinct from the `.sld` files in `src/core/scheme/`, which record what this
 * implementation *claims* to provide. Auditing one against the other is the
 * point; using the `.sld` files as the reference would only prove they agree
 * with themselves.
 *
 * Syntactic keywords are included alongside procedures, since a conforming
 * implementation must provide both, but they are listed separately because
 * they cannot be probed the same way at runtime.
 */

/** Procedures and syntax exported by `(scheme base)`. */
export const SCHEME_BASE = [
  '*', '+', '-', '/', '<', '<=', '=', '>', '>=',
  'abs', 'append', 'apply', 'assoc', 'assq', 'assv',
  'binary-port?', 'boolean=?', 'boolean?',
  'bytevector', 'bytevector-append', 'bytevector-copy', 'bytevector-copy!',
  'bytevector-length', 'bytevector-u8-ref', 'bytevector-u8-set!', 'bytevector?',
  'caar', 'cadr', 'call-with-current-continuation', 'call-with-port',
  'call-with-values', 'call/cc', 'car', 'cdar', 'cddr', 'cdr', 'ceiling',
  'char->integer', 'char-ready?', 'char<=?', 'char<?', 'char=?', 'char>=?',
  'char>?', 'char?', 'close-input-port', 'close-output-port', 'close-port',
  'complex?', 'cons', 'current-error-port', 'current-input-port',
  'current-output-port', 'denominator', 'dynamic-wind',
  'eof-object', 'eof-object?', 'eq?', 'equal?', 'eqv?', 'error',
  'error-object-irritants', 'error-object-message', 'error-object?',
  'even?', 'exact', 'exact-integer-sqrt', 'exact-integer?', 'exact?',
  'expt', 'features', 'file-error?', 'floor', 'floor-quotient',
  'floor-remainder', 'floor/', 'flush-output-port', 'for-each',
  'gcd', 'get-output-bytevector', 'get-output-string',
  'inexact', 'inexact?', 'input-port-open?', 'input-port?',
  'integer->char', 'integer?', 'lcm', 'length',
  'list', 'list->string', 'list->vector', 'list-copy', 'list-ref',
  'list-set!', 'list-tail', 'list?',
  'make-bytevector', 'make-list', 'make-parameter', 'make-string',
  'make-vector', 'map', 'max', 'member', 'memq', 'memv', 'min', 'modulo',
  'negative?', 'newline', 'not', 'null?', 'number->string', 'number?',
  'numerator', 'odd?',
  'open-input-bytevector', 'open-input-string', 'open-output-bytevector',
  'open-output-string', 'output-port-open?', 'output-port?',
  'pair?', 'peek-char', 'port?', 'positive?', 'procedure?',
  'quotient', 'raise', 'raise-continuable', 'rational?', 'rationalize',
  'read-bytevector', 'read-bytevector!', 'read-char', 'read-error?',
  'read-line', 'read-string', 'read-u8', 'real?', 'remainder', 'reverse',
  'round', 'set-car!', 'set-cdr!', 'square',
  'string', 'string->list', 'string->number', 'string->symbol',
  'string->utf8', 'string->vector', 'string-append', 'string-copy',
  'string-copy!', 'string-fill!', 'string-for-each', 'string-length',
  'string-map', 'string-ref', 'string-set!', 'string<=?', 'string<?',
  'string=?', 'string>=?', 'string>?', 'string?', 'substring',
  'symbol->string', 'symbol=?', 'symbol?',
  'truncate', 'truncate-quotient', 'truncate-remainder', 'truncate/',
  'u8-ready?', 'utf8->string',
  'values', 'vector', 'vector->list', 'vector->string', 'vector-append',
  'vector-copy', 'vector-copy!', 'vector-fill!', 'vector-for-each',
  'vector-length', 'vector-map', 'vector-ref', 'vector-set!', 'vector?',
  'with-exception-handler', 'write-bytevector', 'write-char', 'write-string',
  'write-u8', 'zero?'
];

/** Syntactic keywords exported by `(scheme base)`. Probed differently. */
export const SCHEME_BASE_SYNTAX = [
  'and', 'begin', 'case', 'cond', 'cond-expand', 'define',
  'define-record-type', 'define-syntax', 'define-values', 'do', 'else', 'guard', 'if', 'include', 'include-ci',
  'lambda', 'let', 'let*', 'let*-values', 'let-syntax', 'let-values',
  'letrec', 'letrec*', 'letrec-syntax', 'or', 'parameterize', 'quasiquote',
  'quote', 'set!', 'syntax-error', 'syntax-rules', 'unless', 'unquote',
  'unquote-splicing', 'when', '=>', '_', '...'
];

/**
 * Identifiers outside `(scheme base)` that are syntactic keywords rather than
 * procedures. They cannot be probed by evaluating the bare name, so the audit
 * routes them through its syntax check instead. Without this they show up as
 * spurious "missing" results.
 */
export const NON_BASE_SYNTAX = new Set(['case-lambda', 'delay', 'delay-force']);

/** The remaining R7RS-small libraries, keyed by library name. */
export const OTHER_LIBRARIES = {
  '(scheme case-lambda)': ['case-lambda'],
  '(scheme char)': [
    'char-alphabetic?', 'char-ci<=?', 'char-ci<?', 'char-ci=?', 'char-ci>=?',
    'char-ci>?', 'char-downcase', 'char-foldcase', 'char-lower-case?',
    'char-numeric?', 'char-upcase', 'char-upper-case?', 'char-whitespace?',
    'digit-value', 'string-ci<=?', 'string-ci<?', 'string-ci=?',
    'string-ci>=?', 'string-ci>?', 'string-downcase', 'string-foldcase',
    'string-upcase'
  ],
  '(scheme complex)': ['angle', 'imag-part', 'magnitude', 'make-polar',
    'make-rectangular', 'real-part'],
  '(scheme cxr)': [
    'caaar', 'caadr', 'cadar', 'caddr', 'cdaar', 'cdadr', 'cddar', 'cdddr',
    'caaaar', 'caaadr', 'caadar', 'caaddr', 'cadaar', 'cadadr', 'caddar',
    'cadddr', 'cdaaar', 'cdaadr', 'cdadar', 'cdaddr', 'cddaar', 'cddadr',
    'cdddar', 'cddddr'
  ],
  '(scheme eval)': ['environment', 'eval'],
  '(scheme file)': [
    'call-with-input-file', 'call-with-output-file', 'delete-file',
    'file-exists?', 'open-binary-input-file', 'open-binary-output-file',
    'open-input-file', 'open-output-file', 'with-input-from-file',
    'with-output-to-file'
  ],
  '(scheme inexact)': ['acos', 'asin', 'atan', 'cos', 'exp', 'finite?',
    'infinite?', 'log', 'nan?', 'sin', 'sqrt', 'tan'],
  '(scheme lazy)': ['delay', 'delay-force', 'force', 'make-promise', 'promise?'],
  '(scheme load)': ['load'],
  '(scheme process-context)': [
    'command-line', 'emergency-exit', 'exit',
    'get-environment-variable', 'get-environment-variables'
  ],
  '(scheme read)': ['read'],
  '(scheme repl)': ['interaction-environment'],
  '(scheme time)': ['current-jiffy', 'current-second', 'jiffies-per-second'],
  '(scheme write)': ['display', 'write', 'write-shared', 'write-simple']
};

/**
 * `(scheme r5rs)` (R7RS Appendix A): the identifiers R5RS defines, but
 * `transcript-on` and `transcript-off`, with `exact` and `inexact` under their
 * R5RS names. Probed under a prefix, since the same names imported from the
 * other libraries would answer for it otherwise.
 */
export const SCHEME_R5RS = [
  '*', '+', '-', '/', '<', '<=',
  '=', '>', '>=', 'abs', 'acos', 'and',
  'angle', 'append', 'apply', 'asin', 'assoc', 'assq',
  'assv', 'atan', 'begin', 'boolean?', 'caaaar', 'caaadr',
  'caaar', 'caadar', 'caaddr', 'caadr', 'caar', 'cadaar',
  'cadadr', 'cadar', 'caddar', 'cadddr', 'caddr', 'cadr',
  'call-with-current-continuation', 'call-with-input-file', 'call-with-output-file', 'call-with-values', 'car', 'case',
  'cdaaar', 'cdaadr', 'cdaar', 'cdadar', 'cdaddr', 'cdadr',
  'cdar', 'cddaar', 'cddadr', 'cddar', 'cdddar', 'cddddr',
  'cdddr', 'cddr', 'cdr', 'ceiling', 'char->integer', 'char-alphabetic?',
  'char-ci<=?', 'char-ci<?', 'char-ci=?', 'char-ci>=?', 'char-ci>?', 'char-downcase',
  'char-lower-case?', 'char-numeric?', 'char-ready?', 'char-upcase', 'char-upper-case?', 'char-whitespace?',
  'char<=?', 'char<?', 'char=?', 'char>=?', 'char>?', 'char?',
  'close-input-port', 'close-output-port', 'complex?', 'cond', 'cons', 'cos',
  'current-input-port', 'current-output-port', 'define', 'define-syntax', 'delay', 'denominator',
  'display', 'do', 'dynamic-wind', 'else', 'eof-object?', 'eq?',
  'equal?', 'eqv?', 'eval', 'even?', 'exact->inexact', 'exact?',
  'exp', 'expt', 'floor', 'for-each', 'force', 'gcd',
  'if', 'imag-part', 'inexact->exact', 'inexact?', 'input-port?', 'integer->char',
  'integer?', 'interaction-environment', 'lambda', 'lcm', 'length', 'let',
  'let*', 'let-syntax', 'letrec', 'letrec-syntax', 'list', 'list->string',
  'list->vector', 'list-ref', 'list-tail', 'list?', 'load', 'log',
  'magnitude', 'make-polar', 'make-rectangular', 'make-string', 'make-vector', 'map',
  'max', 'member', 'memq', 'memv', 'min', 'modulo',
  'negative?', 'newline', 'not', 'null-environment', 'null?', 'number->string',
  'number?', 'numerator', 'odd?', 'open-input-file', 'open-output-file', 'or',
  'output-port?', 'pair?', 'peek-char', 'positive?', 'procedure?', 'quasiquote',
  'quote', 'quotient', 'rational?', 'rationalize', 'read', 'read-char',
  'real-part', 'real?', 'remainder', 'reverse', 'round', 'scheme-report-environment',
  'set!', 'set-car!', 'set-cdr!', 'sin', 'sqrt', 'string',
  'string->list', 'string->number', 'string->symbol', 'string-append', 'string-ci<=?', 'string-ci<?',
  'string-ci=?', 'string-ci>=?', 'string-ci>?', 'string-copy', 'string-fill!', 'string-length',
  'string-ref', 'string-set!', 'string<=?', 'string<?', 'string=?', 'string>=?',
  'string>?', 'string?', 'substring', 'symbol->string', 'symbol?', 'syntax-rules',
  'tan', 'truncate', 'values', 'vector', 'vector->list', 'vector-fill!',
  'vector-length', 'vector-ref', 'vector-set!', 'vector?', 'with-input-from-file', 'with-output-to-file',
  'write', 'write-char', 'zero?', '=>', '...'
];

/** The syntactic keywords among them. */
export const R5RS_SYNTAX = new Set([
  'and', 'begin', 'case', 'cond', 'define', 'define-syntax',
  'delay', 'do', 'else', 'if', 'lambda', 'let',
  'let*', 'let-syntax', 'letrec', 'letrec-syntax', 'or', 'quasiquote',
  'quote', 'set!', 'syntax-rules', '=>', '...'
]);
