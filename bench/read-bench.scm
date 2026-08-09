(import (liii timeit))

;; Save S7's built-in read before loading our implementation
(define s7-read read)

(load "liii/reader.scm")

(define (read-all read-proc str)
  (call-with-input-string str
    (lambda (port)
      (let loop ((acc '()))
        (let ((d (read-proc port)))
          (if (eof-object? d)
            (reverse acc)
            (loop (cons d acc))))))))

(define (bench-case label str iterations)
  (let ((r1 (read-all s7-read str))
        (r2 (read-all read str)))
    (display label)
    (newline)
    (display "  datums        : ")
    (display (length r1))
    (newline)
    (display "  results equal : ")
    (display (equal? r1 r2))
    (newline)
    (let ((t1 (timeit (lambda () (read-all s7-read str)) '() iterations))
          (t2 (timeit (lambda () (read-all read str)) '() iterations)))
      (display "  s7 read       : ")
      (display t1)
      (display "s")
      (newline)
      (display "  liii read     : ")
      (display t2)
      (display "s")
      (newline)
      (display "  ratio         : ")
      (display (/ t2 t1))
      (newline)
      (newline))))

;; ---------------------------------------------------------------------------
;; sample inputs
;;
;; The samples are restricted to datums that S7's built-in read and this reader
;; parse identically, so that "results equal" is meaningful. S7's read does not
;; support several R7RS forms: #e/#i/#d prefixes, pure imaginary (+i, +2i),
;; polar (1@2), datum comments (#;), |...| identifiers, and it represents
;; quote/backquote differently. NaN is excluded because equal? is #f for it.
;; ---------------------------------------------------------------------------

;; a typical program-ish mix of datums
(define sample-typical
  (string-append
    ";; factorial\n"
    "(define (fact n)\n"
    "  (if (= n 0) 1 (* n (fact (- n 1)))))\n"
    "(display (fact 10)) (newline)\n"
    "\"hello world\" 42 -3.14 1e10 #\\a #\\space #\\newline\n"
    "#(1 2 3) #u8(0 127 255)\n"
    "(let ((lst (list 1 2 3))) (map (lambda (x) (* x x)) lst))\n"
    "22/7 3+4i 2.5e-3 #x2A #b101010 #x-2A .5 5.\n"))

;; number-heavy
(define sample-numbers
  (let loop ((i 0) (acc '()))
    (if (= i 100)
      (apply string-append (reverse acc))
      (loop (+ i 1)
            (cons " 12345 -678 3.14159 2.5e-3 1e10 22/7 -1/2 3+4i 2-i #x2A #o52 #b1010 #x-2A 1e+10 .5 5. +inf.0 -inf.0 "
                  acc)))))

;; deeply nested lists
(define sample-nested
  (let loop ((depth 30))
    (if (= depth 0)
      "42"
      (string-append "(a b " (loop (- depth 1)) " c)"))))

;; the typical sample repeated
(define sample-big
  (let loop ((i 0) (acc '()))
    (if (= i 40)
      (apply string-append (reverse acc))
      (loop (+ i 1) (cons sample-typical acc)))))

;; ---------------------------------------------------------------------------
;; run
;; ---------------------------------------------------------------------------

(bench-case "typical" sample-typical 2000)
(bench-case "numbers" sample-numbers 300)
(bench-case "nested" sample-nested 2000)
(bench-case "big" sample-big 50)
