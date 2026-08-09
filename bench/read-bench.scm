(import (liii timeit))

;; The tiny bootstrap C reader and the Scheme reader from liii/reader.scm.
;; (S7's original C reader was removed when the tiny reader replaced it.)
(define tiny-read g-tiny-read)

(load "liii/reader.scm")

(define (read-all read-proc str)
  (call-with-input-string str
    (lambda (port)
      (let loop ((acc '()))
        (let ((d (read-proc port)))
          (if (eof-object? d)
            (reverse acc)
            (loop (cons d acc))))))))

;; wall-clock is noisy; take the minimum of `runs` timings
(define (best-time thunk iterations runs)
  (let loop ((i 0) (best #f))
    (if (= i runs)
      best
      (let ((t (timeit thunk '() iterations)))
        (loop (+ i 1) (if (or (not best) (< t best)) t best))))))

(define (bench-case label str iterations runs readers)
  (display label)
  (newline)
  (let* ((names (map (lambda (rp) (if (eq? rp tiny-read) "tiny-read" "scheme-read"))
                     readers))
         (results (map (lambda (rp) (read-all rp str)) readers)))
    (display "  datums        : ")
    (display (length (car results)))
    (newline)
    (display "  results equal : ")
    (display (let loop ((rs results))
               (or (null? (cdr rs))
                   (and (equal? (car rs) (cadr rs)) (loop (cdr rs))))))
    (newline)
    (let loop ((ns names) (rps readers))
      (if (null? rps)
        (newline)
        (let ((t (best-time (lambda () (read-all (car rps) str)) iterations runs)))
          (display (string-append "  " (car ns) " : "))
          (display t)
          (display "s")
          (newline)
          (loop (cdr ns) (cdr rps)))))
    (when (> (length readers) 1)
      (let ((t1 (best-time (lambda () (read-all (car readers) str)) iterations runs))
            (t2 (best-time (lambda () (read-all (cadr readers) str)) iterations runs)))
        (display "  tiny/scheme   : ")
        (display (/ t2 t1))
        (newline)))
      (newline)))

;; ---------------------------------------------------------------------------
;; sample inputs
;;
;; The "simple" samples are restricted to datums the tiny reader supports
;; (integers, symbols, strings, booleans, chars, lists, quote, comments), so
;; that both readers parse them identically. The full-R7RS samples are parsed
;; by the Scheme reader only.
;; ---------------------------------------------------------------------------

(define sample-simple
  (string-append
    ";; a program-ish mix that the tiny reader can handle\n"
    "(define (fact n)\n"
    "  (if (= n 0) 1 (* n (fact (- n 1)))))\n"
    "(display (fact 10)) (newline)\n"
    "\"hello world\" 42 -3 1000 #\\a #\\space #\\newline #\\x41\n"
    "(a b c (d e) (f . g)) (quote x) (if #t #f #t)\n"
    "sym-with-dashes _start +end -3 +42\n"
    "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20\n"))

;; a typical program-ish mix of datums (full R7RS)
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

(bench-case "simple (tiny-compatible)" sample-simple 5000 3
            (list tiny-read read))
(bench-case "nested (tiny-compatible)" sample-nested 5000 3
            (list tiny-read read))
(bench-case "typical" sample-typical 5000 3
            (list read))
(bench-case "numbers" sample-numbers 500 3
            (list read))
(bench-case "big" sample-big 100 3
            (list read))
