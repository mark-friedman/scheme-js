;; events.scm -- a page's event handlers, registered once and called many times.
;;
;; Synthetic, written for `benchmarks/run_tier.js` to stand for the shape of a
;; page's code that the canonical benchmarks lack: a setup procedure, called
;; once, makes the handlers -- closures over the page's state -- and registers
;; them, and from then on the browser calls them, one event at a time. Here a
;; shopping cart's buttons and inputs, and a stream of events made by a
;; pseudo-random generator so that the run is the same every time.

(import (scheme base) (scheme write))

;; ---------------------------------------------------------------------------
;; The page's state
;; ---------------------------------------------------------------------------

(define-record-type item
  (make-item name price quantity)
  item?
  (name item-name)
  (price item-price)
  (quantity item-quantity set-item-quantity!))

(define-record-type cart
  (make-cart items discount log)
  cart?
  (items cart-items set-cart-items!)
  (discount cart-discount set-cart-discount!)
  (log cart-log set-cart-log!))

;; /**
;;  * The integers from 0 below n.
;;  * @param {integer} n - How many.
;;  * @returns {list}
;;  */
(define (iota-list n)
  (let loop ((k (- n 1)) (acc '()))
    (if (< k 0) acc (loop (- k 1) (cons k acc)))))

;; /**
;;  * The catalogue a page shows: a name and a price in cents for each product.
;;  */
(define catalogue
  (list->vector
    (map (lambda (k) (cons (string-append "product-" (number->string k)) (+ 199 (* 37 (modulo (* k 7) 23)))))
         (iota-list 40))))

;; ---------------------------------------------------------------------------
;; What the handlers do
;; ---------------------------------------------------------------------------

;; /**
;;  * The cart's item of a product, or #f.
;;  * @param {cart} cart - The cart.
;;  * @param {string} name - The product.
;;  * @returns {item|boolean}
;;  */
(define (find-item cart name)
  (let loop ((items (cart-items cart)))
    (cond ((null? items) #f)
          ((string=? (item-name (car items)) name) (car items))
          (else (loop (cdr items))))))

;; /**
;;  * Adds one of a product to the cart.
;;  * @param {cart} cart - The cart.
;;  * @param {pair} product - Its name and price.
;;  */
(define (add-to-cart! cart product)
  (let ((existing (find-item cart (car product))))
    (if existing
        (set-item-quantity! existing (+ 1 (item-quantity existing)))
        (set-cart-items! cart (cons (make-item (car product) (cdr product) 1) (cart-items cart))))))

;; /**
;;  * Takes a product out of the cart.
;;  * @param {cart} cart - The cart.
;;  * @param {string} name - The product.
;;  */
(define (remove-from-cart! cart name)
  (set-cart-items! cart (let loop ((items (cart-items cart)) (kept '()))
                          (cond ((null? items) (reverse kept))
                                ((string=? (item-name (car items)) name) (loop (cdr items) kept))
                                (else (loop (cdr items) (cons (car items) kept)))))))

;; /**
;;  * Sets how many of a product the cart holds, taking it out at none.
;;  * @param {cart} cart - The cart.
;;  * @param {string} name - The product.
;;  * @param {integer} quantity - How many.
;;  */
(define (set-quantity! cart name quantity)
  (let ((existing (find-item cart name)))
    (if existing
        (if (<= quantity 0)
            (remove-from-cart! cart name)
            (set-item-quantity! existing quantity)))))

;; /**
;;  * The cart's price before its discount, in cents.
;;  * @param {cart} cart - The cart.
;;  * @returns {integer}
;;  */
(define (subtotal cart)
  (let loop ((items (cart-items cart)) (sum 0))
    (if (null? items)
        sum
        (loop (cdr items) (+ sum (* (item-price (car items)) (item-quantity (car items))))))))

;; /**
;;  * The cart's price after its discount, in cents.
;;  * @param {cart} cart - The cart.
;;  * @returns {integer}
;;  */
(define (total cart)
  (let ((sub (subtotal cart)))
    (- sub (quotient (* sub (cart-discount cart)) 100))))

;; /**
;;  * A price as a page shows it.
;;  * @param {integer} cents - The price.
;;  * @returns {string}
;;  */
(define (format-cents cents)
  (let ((dollars (quotient cents 100)) (rest (remainder cents 100)))
    (string-append "$" (number->string dollars) "." (if (< rest 10) "0" "") (number->string rest))))

;; /**
;;  * The cart's summary line.
;;  * @param {cart} cart - The cart.
;;  * @returns {string}
;;  */
(define (render-summary cart)
  (string-append (number->string (length (cart-items cart))) " items, " (format-cents (total cart))))

;; /**
;;  * Records what happened, keeping the last twenty or so.
;;  * @param {cart} cart - The cart.
;;  * @param {string} text - What happened.
;;  */
(define (note! cart text)
  (let ((log (cart-log cart)))
    (set-cart-log! cart (if (> (length log) 20) (list text) (cons text log)))))

;; ---------------------------------------------------------------------------
;; Setting the page up: the handlers, made once
;; ---------------------------------------------------------------------------

;; /**
;;  * Makes the page's handlers over a cart, as a page's start-up code would,
;;  * and returns them by the event each handles.
;;  * @param {cart} cart - The page's cart.
;;  * @returns {list} An association list from event names to handlers.
;;  */
(define (install-handlers cart)
  (let ((renders 0))
    (list
      (cons 'add (lambda (k)
                   (add-to-cart! cart (vector-ref catalogue (modulo k (vector-length catalogue))))
                   (note! cart "added")))
      (cons 'remove (lambda (k)
                      (let ((items (cart-items cart)))
                        (if (pair? items)
                            (remove-from-cart! cart (item-name (list-ref items (modulo k (length items)))))))
                      (note! cart "removed")))
      (cons 'quantity (lambda (k)
                        (let ((items (cart-items cart)))
                          (if (pair? items)
                              (set-quantity! cart (item-name (list-ref items (modulo k (length items))))
                                             (modulo k 5))))))
      (cons 'coupon (lambda (k) (set-cart-discount! cart (modulo k 30))))
      (cons 'render (lambda (k)
                      (set! renders (+ renders 1))
                      (string-length (render-summary cart))))
      (cons 'count (lambda (k) renders)))))

;; ---------------------------------------------------------------------------
;; The browser: events, one at a time
;; ---------------------------------------------------------------------------

;; /**
;;  * The next number of a linear congruential generator, so that the events
;;  * are the same every run.
;;  * @param {integer} seed - The last number.
;;  * @returns {integer}
;;  */
(define (next-random seed) (modulo (+ (* seed 1103515245) 12345) 2147483648))

;; /**
;;  * The events, as often as each happens.
;;  */
(define event-kinds #(add add add remove quantity coupon render render))

;; /**
;;  * Dispatches a number of events to the handlers, as the browser would.
;;  * @param {list} handlers - The handlers, by event.
;;  * @param {integer} n - How many events.
;;  * @returns {integer} A checksum of what the render handler returned.
;;  */
(define (run-events handlers n)
  (let loop ((i 0) (seed 42) (checksum 0))
    (if (= i n)
        checksum
        (let* ((seed (next-random seed))
               (kind (vector-ref event-kinds (modulo (quotient seed 65536) (vector-length event-kinds))))
               (handler (cdr (assq kind handlers)))
               (result (handler (quotient seed 256))))
          (loop (+ i 1) seed (if (and (eq? kind 'render) (integer? result))
                                 (modulo (+ checksum result i) 1000003)
                                 checksum))))))

(define page-cart (make-cart '() 0 '()))
(define handlers (install-handlers page-cart))
(define checksum (run-events handlers 20000))
(display (list 'events checksum ((cdr (assq 'count handlers)) 0) (render-summary page-cart)))
(newline)
