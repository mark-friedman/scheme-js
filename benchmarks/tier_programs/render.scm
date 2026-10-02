;; render.scm -- a page rendered from its data, a few times.
;;
;; Synthetic, written for `benchmarks/run_tier.js` to stand for the shape of a
;; page's code that the canonical benchmarks lack: many small procedures, a
;; template each, called a few times or once per item, with no loop of their
;; own to speak of. A store's product listing is rendered to HTML text, its
;; filters change, and it is rendered again.

(import (scheme base) (scheme char) (scheme write))

;; ---------------------------------------------------------------------------
;; The data
;; ---------------------------------------------------------------------------

(define-record-type product
  (make-product id name category price rating stock tags)
  product?
  (id product-id)
  (name product-name)
  (category product-category)
  (price product-price)
  (rating product-rating)
  (stock product-stock)
  (tags product-tags))

;; /**
;;  * The categories products belong to.
;;  */
(define categories #("kitchen" "garden" "office" "toys" "books" "audio"))

;; /**
;;  * The tags a product may have.
;;  */
(define all-tags #("new" "sale" "eco" "gift" "bulk" "local"))

;; /**
;;  * A product made from its number, the same every run.
;;  * @param {integer} k - Its number.
;;  * @returns {product}
;;  */
(define (product-number k)
  (make-product k
                (string-append "Item & \"" (number->string k) "\" <" (vector-ref categories (modulo k 6)) ">")
                (vector-ref categories (modulo (* k 5) 6))
                (+ 500 (* 113 (modulo (* k 31) 97)))
                (modulo (* k 7) 6)
                (modulo (* k 13) 40)
                (let loop ((t 0) (acc '()))
                  (cond ((= t 6) acc)
                        ((zero? (modulo (+ k t) 3)) (loop (+ t 1) (cons (vector-ref all-tags t) acc)))
                        (else (loop (+ t 1) acc))))))

;; /**
;;  * The store's products.
;;  */
(define products
  (let loop ((k 299) (acc '()))
    (if (< k 0) acc (loop (- k 1) (cons (product-number k) acc)))))

;; ---------------------------------------------------------------------------
;; Text
;; ---------------------------------------------------------------------------

;; /**
;;  * Text with HTML's special characters escaped.
;;  * @param {string} text - The text.
;;  * @returns {string}
;;  */
(define (escape-html text)
  (let ((out (open-output-string)))
    (string-for-each
      (lambda (c)
        (case c
          ((#\<) (write-string "&lt;" out))
          ((#\>) (write-string "&gt;" out))
          ((#\&) (write-string "&amp;" out))
          ((#\") (write-string "&quot;" out))
          (else (write-char c out))))
      text)
    (get-output-string out)))

;; /**
;;  * An element: its tag, attributes and content.
;;  * @param {string} tag - The tag.
;;  * @param {list} attributes - Pairs of attribute names and values.
;;  * @param {...string} content - Its content, already HTML.
;;  * @returns {string}
;;  */
(define (element tag attributes . content)
  (string-append "<" tag (render-attributes attributes) ">" (apply string-append content) "</" tag ">"))

;; /**
;;  * Attributes as HTML.
;;  * @param {list} attributes - Pairs of names and values.
;;  * @returns {string}
;;  */
(define (render-attributes attributes)
  (apply string-append
         (map (lambda (a) (string-append " " (car a) "=\"" (escape-html (cdr a)) "\"")) attributes)))

;; /**
;;  * A price as a page shows it.
;;  * @param {integer} cents - The price.
;;  * @returns {string}
;;  */
(define (format-price cents)
  (let ((rest (remainder cents 100)))
    (string-append "$" (number->string (quotient cents 100)) "." (if (< rest 10) "0" "") (number->string rest))))

;; /**
;;  * A rating as stars.
;;  * @param {integer} rating - From 0 to 5.
;;  * @returns {string}
;;  */
(define (format-rating rating)
  (string-append (make-string rating #\*) (make-string (- 5 rating) #\.)))

;; /**
;;  * A word with its first letter capitalised.
;;  * @param {string} word - The word.
;;  * @returns {string}
;;  */
(define (capitalise word)
  (if (string=? word "")
      word
      (string-append (string (char-upcase (string-ref word 0))) (substring word 1 (string-length word)))))

;; ---------------------------------------------------------------------------
;; The templates
;; ---------------------------------------------------------------------------

;; /**
;;  * How much of a product is left, as a page says it.
;;  * @param {integer} stock - How many.
;;  * @returns {string}
;;  */
(define (stock-label stock)
  (cond ((zero? stock) (element "span" '(("class" . "out")) "Sold out"))
        ((< stock 5) (element "span" '(("class" . "low")) "Only " (number->string stock) " left"))
        (else (element "span" '(("class" . "in")) "In stock"))))

;; /**
;;  * A product's tags.
;;  * @param {list} tags - The tags.
;;  * @returns {string}
;;  */
(define (tag-list tags)
  (element "ul" '(("class" . "tags"))
           (apply string-append (map (lambda (t) (element "li" '() (escape-html t))) tags))))

;; /**
;;  * One product's card.
;;  * @param {product} p - The product.
;;  * @returns {string}
;;  */
(define (product-card p)
  (element "div" (list (cons "class" "card") (cons "data-id" (number->string (product-id p))))
           (element "h3" '() (escape-html (product-name p)))
           (element "p" '(("class" . "price")) (format-price (product-price p)))
           (element "p" '(("class" . "rating")) (format-rating (product-rating p)))
           (stock-label (product-stock p))
           (tag-list (product-tags p))))

;; /**
;;  * The heading of a category's section.
;;  * @param {string} category - The category.
;;  * @param {integer} count - How many products it shows.
;;  * @returns {string}
;;  */
(define (section-heading category count)
  (element "h2" '() (capitalise category) " (" (number->string count) ")"))

;; /**
;;  * A category's section.
;;  * @param {string} category - The category.
;;  * @param {list} shown - The products shown in it.
;;  * @returns {string}
;;  */
(define (category-section category shown)
  (element "section" (list (cons "id" category))
           (section-heading category (length shown))
           (apply string-append (map product-card shown))))

;; /**
;;  * The filter bar, saying what is filtered.
;;  * @param {filters} f - The filters.
;;  * @returns {string}
;;  */
(define (filter-bar f)
  (element "nav" '(("class" . "filters"))
           (element "span" '() "Under " (format-price (filters-max-price f)))
           (element "span" '() "At least " (format-rating (filters-min-rating f)))
           (if (filters-in-stock? f) (element "span" '() "In stock only") "")))

;; /**
;;  * The page's header.
;;  * @param {string} title - The page's title.
;;  * @returns {string}
;;  */
(define (page-header title)
  (element "header" '() (element "h1" '() (escape-html title)) (element "a" '(("href" . "/cart")) "Cart")))

;; /**
;;  * The page's footer.
;;  * @param {integer} shown - How many products are shown.
;;  * @returns {string}
;;  */
(define (page-footer shown)
  (element "footer" '() "Showing " (number->string shown) " of " (number->string (length products))))

;; ---------------------------------------------------------------------------
;; Filtering, and the page
;; ---------------------------------------------------------------------------

(define-record-type filters
  (make-filters max-price min-rating in-stock?)
  filters?
  (max-price filters-max-price)
  (min-rating filters-min-rating)
  (in-stock? filters-in-stock?))

;; /**
;;  * Whether a product passes the filters.
;;  * @param {filters} f - The filters.
;;  * @param {product} p - The product.
;;  * @returns {boolean}
;;  */
(define (shown? f p)
  (and (<= (product-price p) (filters-max-price f))
       (>= (product-rating p) (filters-min-rating f))
       (or (not (filters-in-stock? f)) (> (product-stock p) 0))))

;; /**
;;  * The products of a category that pass the filters.
;;  * @param {filters} f - The filters.
;;  * @param {string} category - The category.
;;  * @returns {list}
;;  */
(define (shown-in f category)
  (let loop ((ps products) (acc '()))
    (cond ((null? ps) (reverse acc))
          ((and (string=? (product-category (car ps)) category) (shown? f (car ps)))
           (loop (cdr ps) (cons (car ps) acc)))
          (else (loop (cdr ps) acc)))))

;; /**
;;  * The whole page, for some filters.
;;  * @param {filters} f - The filters.
;;  * @returns {string}
;;  */
(define (render-page f)
  (let* ((sections (map (lambda (c) (cons c (shown-in f c))) (vector->list categories)))
         (shown (apply + (map (lambda (s) (length (cdr s))) sections))))
    (element "html" '()
             (page-header "The Store")
             (filter-bar f)
             (apply string-append (map (lambda (s) (category-section (car s) (cdr s))) sections))
             (page-footer shown))))

;; /**
;;  * A checksum of text, so that the run's output is short.
;;  * @param {string} text - The text.
;;  * @returns {integer}
;;  */
(define (checksum text)
  (let loop ((i 0) (sum 0))
    (if (= i (string-length text))
        sum
        (loop (+ i 1) (modulo (+ (* sum 31) (char->integer (string-ref text i))) 1000003)))))

;; The page as first shown, then as the visitor changes the filters.
(define renders
  (map render-page
       (list (make-filters 100000 0 #f)
             (make-filters 6000 2 #f)
             (make-filters 6000 2 #t)
             (make-filters 9000 4 #t))))
(display (list 'render (map string-length renders) (map checksum renders)))
(newline)
