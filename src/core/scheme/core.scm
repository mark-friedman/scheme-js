; Core Bootstrap Module
; 
; This file is a container/index for the core Scheme procedures.
; The actual implementations are split into separate files for organization:
;
;   macros.scm     - Core macros: and, let, letrec, cond, define-record-type
;   equality.scm   - equal? deep equality
;   cxr.scm        - All 28 car/cdr composition accessors
;   numbers.scm    - Numeric comparisons, predicates, min/max, gcd/lcm, round
;   list.scm       - List procedures: map, for-each, memq/v, assq/v, length, etc.
;
; The test runner loads these files individually in the correct order.
; For library-based loading, see base.sld and core.sld.
;
; NOTE: This file is kept for documentation purposes and backwards compatibility.
; If loaded directly, it does nothing because the content is in separate files.
