(test-group "dynamic-wind interop"

  ;; Setup: Create JS helpers using js-eval
  (js-eval "
    window.log = [];
    window.invokeCallback = (cb) => cb();
    window.invokeValues = (cb, ...args) => cb(...args);
    window.storeK = null;
    window.invokeK = (val) => { 
        if(window.storeK) window.storeK(val); 
    };
    window.runSchemeDW = (fn) => fn();
  ")

  (define (get-js-log) (js-eval "window.log"))
  (define (clear-js-log) (js-eval "window.log = []"))
  (define (log-to-js val) (js-eval (string-append "window.log.push('" (symbol->string val) "')")))

  ;; Test 1: Scheme -> JS -> Scheme (Re-entry via Callback)
  (test "Scheme -> JS -> Scheme (Re-entry)"
        '(before in after)
        (begin
          (clear-js-log)
          ((js-eval "window.invokeCallback") 
           (lambda ()
               (dynamic-wind
                 (lambda () (log-to-js 'before))
                 (lambda () (log-to-js 'in))
                 (lambda () (log-to-js 'after)))))
          (let ((vec (get-js-log)))
             (if (vector? vec) 
                 (map string->symbol (vector->list vec))
                 vec))))

  ;; Test 2: Scheme -> JS -> Scheme (Standard, with call/cc return)
  (test "Scheme -> JS -> Scheme (Standard)"
        '(after in before)
        (let ((log '()))
          (call-with-current-continuation
            (lambda (return)
              ((js-eval "window.invokeCallback")
               (lambda ()
                 (dynamic-wind
                   (lambda () (set! log (cons 'before log)))
                   (lambda () (set! log (cons 'in log)))
                   (lambda () (set! log (cons 'after log))))))))
          log))

  ;; Test 3: Continuation Escaping via JavaScript
  (test "JS -> Escape"
        '(after in before)
        (let ((log '()))
           (call-with-current-continuation
             (lambda (k)
               (dynamic-wind
                 (lambda () (set! log (cons 'before log)))
                 (lambda ()
                   (set! log (cons 'in log))
                   ;; Call JS, passing k. JS calls k.
                   ((js-eval "window.invokeValues") k 'escape)) 
                 (lambda () (set! log (cons 'after log))))))
           log))

  ;; Test 4: Re-entry into Dynamic Extent from JavaScript
  (test "JS -> Re-entry (Global Stored K)"
        '(after before after in before) ; Expected log sequence
        (let ((log '())
              (count 0))
          (call-with-current-continuation
            (lambda (exit)
              ;; 1. Run dynamic-wind, capture K, store in JS, and exit.
              (dynamic-wind
                (lambda () (set! log (cons 'before log)))
                (lambda () 
                   (set! log (cons 'in log))
                   (call-with-current-continuation
                     (lambda (k)
                       (if (= count 0)
                           (begin 
                             (set! count 1)
                             ;; Store in JS global
                             ((js-eval "k => window.storeK = k") k)
                             ;; Exit first run normally? No, let's just finish the DW.
                             )
                           (begin
                             ;; Re-entry point
                             (exit 'done)
                           )))))
                (lambda () (set! log (cons 'after log))))
              
              ;; 2. Invoke from JS
              ;; We only do this ONCE (when count is 1)
              (if (= count 1)
                  (begin
                     (set! count 2)
                     ((js-eval "() => window.invokeK('reentry')"))))))
          
          log))

  ;; Test 5: Interleaved Calls (Scheme -> JS -> Scheme -> Escape/Re-enter)
  (test "Interleaved Calls"
        '(after before after before)
        (let ((log '()))
           (call-with-current-continuation
             (lambda (escape)
                (let ((k 
                       (call-with-current-continuation
                         (lambda (return-k-to-top)
                            ;; Call JS -> Scheme DW
                            ((js-eval "fn => fn()") 
                             (lambda ()
                               (dynamic-wind
                                 (lambda () (set! log (cons 'before log)))
                                 (lambda () 
                                    ;; Capture K inside DW
                                    (call-with-current-continuation
                                       (lambda (inner-k)
                                          ;; Return K to top level (bypassing JS return)
                                          (return-k-to-top inner-k))))
                                 (lambda () (set! log (cons 'after log))))))))))
                  
                  (if (string? k)
                      log
                      (k "second-pass")))))))
)
