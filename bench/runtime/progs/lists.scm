(import (scheme base) (scheme write))
;; List pipeline: mass consing, higher-order map/filter/fold, assoc lookups.
;; Allocation pressure plus the HOF/primitive call path.  Sizes are tuned so
;; the whole program stays around a couple of seconds (assoc is linear).
(define (build n) (let loop ((i 0) (a '())) (if (= i n) a (loop (+ i 1) (cons i a)))))
(define data (build 300000))
(define evens (filter (lambda (x) (= 0 (modulo x 2))) data))
(define mapped (map (lambda (x) (* x 3)) evens))
(define total (fold + 0 mapped))
(define table (map (lambda (i) (cons (* i 7) (quote sym))) (build 2000)))
(define (lookups n)
  (let loop ((i 0) (hits 0))
    (if (= i n)
        hits
        (loop (+ i 1)
              (if (assq (modulo (* i 7) 14000) table) (+ hits 1) hits)))))
(write total) (display " ") (write (lookups 20000)) (newline)
