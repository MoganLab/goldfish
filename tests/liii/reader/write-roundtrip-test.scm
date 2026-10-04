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

;; A small shared graph represents exponentially many paths. Writing and
;; reading it must preserve each alias without expanding those paths.
(let* ((depth 24)
       (graph (let loop ((n depth) (node 'leaf))
                (if (zero? n)
                  node
                  (loop (- n 1)
                        (if (even? n) (cons node node) (vector node node))))))
       (restored (roundtrip graph)))
  (check (let loop ((n depth) (node restored))
           (if (zero? n)
             (eq? node 'leaf)
             (let ((left (if (pair? node) (car node) (vector-ref node 0)))
                   (right (if (pair? node) (cdr node) (vector-ref node 1))))
               (and (eq? left right) (loop (- n 1) left)))))
         => #t))

;; Growing the memo tables must preserve aliases and mutation visibility.
(let ((nodes (make-vector 600)))
  (let fill ((i 0))
    (when (< i 300)
      (let ((node (vector i i)))
        (vector-set! nodes (* 2 i) node)
        (vector-set! nodes (+ 1 (* 2 i)) node))
      (fill (+ i 1))))
  (let ((restored (roundtrip (list nodes nodes))))
    (check (eq? (car restored) (cadr restored)) => #t)
    (check (vector-ref (car restored) 599) => #(299 299))
    (vector-set! (vector-ref (car restored) 598) 0 'changed)
    (check (vector-ref (vector-ref (cadr restored) 599) 0) => 'changed)))

(let ((cycle (read (open-input-string "#0=(1 . #0#)"))))
  (check (eq? cycle (cdr cycle)) => #t))
(let ((cycle (read (open-input-string "#0=#(1 #0#)"))))
  (check (eq? cycle (vector-ref cycle 1)) => #t))

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
