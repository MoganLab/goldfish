;;; base-functions.scm -- library functions implemented in Scheme.
;;;
;;; Loaded into the ROOTLET (like Guile's boot-9.scm into the root module)
;;; right after the module system comes up, overriding the s7 primitives of
;;; the same name.  All code -- the expander kernel itself, every library,
;;; and user programs -- resolves `map' / `for-each' by name into the
;;; rootlet, so this single Scheme definition is what everyone calls.  The
;;; multi-list variants use the `apply' primitive for variadic callback
;;; calls (Guile boot-9 style); the rest-list iteration uses explicit
;;; helper recursion so the definition does not depend on `map' being
;;; bound before it is installed.
;;;
;;; Multiple values are a derived form at the COMPILER level only:
;;; values / call-with-values are ordinary host procedures (no IR
;;; nodes, no VM opcodes) -- the host's values objects and apply
;;; splicing ARE the representation.  Nothing is redefined here.

;; These are library semantics, not evaluator primitives.  Keep only pair and
;; vector construction/access in the native substrate.
(define (negative? x) (< x 0))
(define (boolean=? x y) (and (boolean? x) (boolean? y) (eq? x y)))
(define (odd? x) (not (= (modulo x 2) 0)))
(define (even? x) (= (modulo x 2) 0))
(define (abs x) (if (negative? x) (- 0 x) x))
(define (make-list n . fill)
  (if (= n 0) '()
      (cons (if (null? fill) #f (car fill))
            (make-list (- n 1) (if (null? fill) #f (car fill))))))
(define (list-copy x)
  (if (null? x) '() (cons (car x) (list-copy (cdr x)))))
(define (list-tail x n)
  (if (= n 0) x (list-tail (cdr x) (- n 1))))
(define (list-ref x n) (car (list-tail x n)))
(define (member x xs . maybe-equal?)
  (let ((same? (if (null? maybe-equal?) equal? (car maybe-equal?))))
    (if (null? xs) #f
        (if (same? x (car xs)) xs
            (member x (cdr xs) same?)))))
(define (assoc x xs . maybe-equal?)
  (let ((same? (if (null? maybe-equal?) equal? (car maybe-equal?))))
    (if (null? xs) #f
        (if (same? x (caar xs)) (car xs)
            (assoc x (cdr xs) same?)))))
(define (vector->list v)
  (let loop ((i 0) (out '()))
    (if (= i (vector-length v)) (reverse out)
        (loop (+ i 1) (cons (vector-ref v i) out)))))
(define (list->vector xs)
  (let ((v (make-vector (length xs))))
    (let loop ((i 0) (rest xs))
      (if (null? rest) v
          (begin (vector-set! v i (car rest))
                 (loop (+ i 1) (cdr rest)))))))
(define (vector-fill! v x)
  (let loop ((i 0))
    (if (= i (vector-length v)) (if #f #f)
        (begin (vector-set! v i x) (loop (+ i 1))))))



;; every one of the rest lists still has an element: R7RS multi-list
;; map / for-each stop at the shortest list, so the loop guard must
;; check them all, not just l1 (zip relies on this for ragged input).
(define (lists-live? rest)
  (if (null? rest)
    #t
    (and (pair? (car rest)) (lists-live? (cdr rest)))))

(define (map f l1 . rest)
  (if (null? rest)
    (let map1 ((l l1))
      (if (pair? l)
        (cons (f (car l)) (map1 (cdr l)))
        '()))
    (let mapn ((l1 l1) (rest rest))
      (if (and (pair? l1) (lists-live? rest))
        (cons (apply f (car l1) (map-cars rest))
              (mapn (cdr l1) (map-cdrs rest)))
        '()))))

;; car of every rest list.
(define (map-cars rest)
  (if (null? rest)
    '()
    (cons (caar rest) (map-cars (cdr rest)))))

;; cdr of every rest list.
(define (map-cdrs rest)
  (if (null? rest)
    '()
    (cons (cdar rest) (map-cdrs (cdr rest)))))

(define (for-each f l1 . rest)
  (if (null? rest)
    (let fe1 ((l l1))
      (if (not (null? l))
        (begin
          (f (car l))
          (fe1 (cdr l)))))
    (let fen ((l1 l1) (rest rest))
      (if (and (not (null? l1)) (lists-live? rest))
        (begin
          (apply f (car l1) (map-cars rest))
          (fen (cdr l1) (map-cdrs rest)))))))


;; Fold follows the conventional (accumulator element) calling order used by
;; the expander's set helpers and SRFI-1.
