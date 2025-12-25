;; (scheme time) library
;;
;; R7RS time procedures using JavaScript Date.

(define-library (scheme time)
  (import (scheme base))
  (export current-second current-jiffy jiffies-per-second)
  (begin
    ;; /**
    ;;  * current-second - Returns current time as seconds since epoch.
    ;;  * @returns {real} Seconds since 1970-01-01 00:00:00 UTC.
    ;;  */
    ;; Implemented as JS primitive
    
    ;; /**
    ;;  * current-jiffy - Returns current time in jiffies.
    ;;  * A jiffy is 1 millisecond in our implementation.
    ;;  * @returns {integer} Current jiffy count.
    ;;  */
    ;; Implemented as JS primitive
    
    ;; /**
    ;;  * jiffies-per-second - Returns the number of jiffies per second.
    ;;  * @returns {integer} 1000 (milliseconds per second).
    ;;  */
    ;; Implemented as JS primitive
    ))
