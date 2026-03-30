(import (scheme base) (scheme-js interop))

(define blockly "blockly-placeholder")
(define generator "gen-placeholder")
(define my-counter 42)

(define (setup-generate-button)
  (let ((btn (document.getElementById "blocklyButton")))
    (when btn
      (btn.addEventListener "click"
        (lambda (event)
          (console.log "clicked"))))))

(setup-generate-button)

(console.log "App loaded" my-counter (+ 1 2))
