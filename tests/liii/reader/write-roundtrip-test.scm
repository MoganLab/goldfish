(import (liii check))
(import (goldfish))

;; ---------------------------------------------------------------------------
;; write-roundtrip hardening.
;;
;; The writer must not hang on cyclic structure: record-free cyclic data is
;; rejected with a clear error (the single-pass cache path emits no labels,
;; and tiny-reader caches cannot carry them), while acyclic data keeps its
;; label-free fast path.  Record-bearing payloads still take the graph
;; writer, whose spine loop must terminate on shared tails.
;; ---------------------------------------------------------------------------

(define (serialize x)
  (let ((p (open-output-string)))
    (write-roundtrip x p)
    (get-output-string p)))

(define (roundtrip x)
  (call-with-input-string (serialize x) read))

;; Plain acyclic data round-trips, including bar-quoted symbols and vectors.
(check (roundtrip '(a b (c) #(1 2) "s")) => '(a b (c) #(1 2) "s"))
(check (roundtrip '|lambda with dots|) => '|lambda with dots|)
(check (roundtrip '((a b) . c)) => '((a b) . c))
(check (roundtrip #(1 (2 3) #(4))) => #(1 (2 3) #(4)))

;; Shared but acyclic data round-trips (duplicated by the single-pass writer).
(let* ((tail (list 2 3))
       (x (list 1 tail)))
  (check (roundtrip x) => '(1 (2 3))))

;; Cyclic structure without records is a serialization error, not a hang.
(define (serialize-error? thunk)
  (catch #t
    (lambda ()
      (thunk)
      #f)
    (lambda (tag . args) #t)))

(let ((c (list 1)))
  (set-cdr! c c)
  (check (serialize-error? (lambda () (serialize c))) => #t))

(let* ((a (list 'a #f))
       (b (list 'b a)))
  (set-car! (cdr a) b)
  (check (serialize-error? (lambda () (serialize a))) => #t))

(let ((v (vector 'v #f)))
  (vector-set! v 1 v)
  (check (serialize-error? (lambda () (serialize v))) => #t))

;; Live procedures are refused on both paths.
(check (serialize-error? (lambda () (serialize (list 1 (lambda (x) x))))) => #t)

(check-report)
