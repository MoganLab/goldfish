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
;;
;; The checks here are the error contract the test suite pins down: wrong
;; arity, wrong type and out-of-range must raise instead of silently
;; looping (make-list/list-tail with a negative count used to recurse
;; forever) or quietly truncating.
(define (negative? x) (< x 0))
(define (boolean=? x y . rest)
  (let same? ((objs (cons y rest)) (first x))
    (if (null? objs)
      #t
      (and (boolean? first) (boolean? (car objs)) (eq? first (car objs))
           (same? (cdr objs) first)))))
(define (odd? x)
  (if (not (integer? x))
    (error 'wrong-type-arg "odd?: expected an integer" x))
  (not (= (modulo x 2) 0)))
(define (even? x)
  (if (not (integer? x))
    (error 'wrong-type-arg "even?: expected an integer" x))
  (= (modulo x 2) 0))
(define (abs x) (if (negative? x) (- 0 x) x))
(define (make-list n . fill)
  (if (not (integer? n))
    (error 'wrong-type-arg "make-list: expected an integer length" n))
  (if (and (pair? fill) (pair? (cdr fill)))
    (error 'wrong-number-of-args "make-list: at most one fill value"))
  (if (< n 0)
    (error 'out-of-range "make-list: length must be non-negative" n))
  (let loop ((i n) (out '()))
    (if (= i 0)
      (reverse out)
      (loop (- i 1) (cons (if (null? fill) #f (car fill)) out)))))
(define (list-copy x)
  (if (pair? x) (cons (car x) (list-copy (cdr x))) x))
(define (list-tail x n)
  (if (not (or (pair? x) (null? x)))
    (error 'wrong-type-arg "list-tail: expected a list" x))
  (if (not (integer? n))
    (error 'wrong-type-arg "list-tail: expected an integer index" n))
  (let loop ((rest x) (i n))
    (cond ((< i 0) (error 'out-of-range "list-tail: index out of range" n))
          ((= i 0) rest)
          ((pair? rest) (loop (cdr rest) (- i 1)))
          (else (error 'out-of-range "list-tail: index out of range" n)))))
;; An index that walked off a non-empty list is out-of-range; an empty
;; list keeps car's wrong-type-arg, which the contract pins separately.
(define (list-ref x n)
  (let ((tail (list-tail x n)))
    (if (and (null? tail) (pair? x))
      (error 'out-of-range "list-ref: index out of range" n)
      (car tail))))
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
(define (vector->list v . range)
  (if (not (vector? v))
    (error 'wrong-type-arg "vector->list: expected a vector" v))
  (if (pair? range)
    (if (pair? (cdr range))
      (if (pair? (cddr range))
        (error 'wrong-number-of-args "vector->list: at most two indices"))))
  (let* ((len (vector-length v))
         (start (if (null? range) 0 (car range)))
         (end (if (or (null? range) (null? (cdr range))) len (cadr range))))
    (if (or (not (integer? start)) (not (integer? end)))
      (error 'wrong-type-arg "vector->list: expected integer indices" start end))
    (if (or (< start 0) (< end start) (> end len))
      (error 'out-of-range "vector->list: index out of range" start end))
    (let loop ((i start) (out '()))
      (if (>= i end)
        (reverse out)
        (loop (+ i 1) (cons (vector-ref v i) out))))))
(define (list->vector xs)
  (let ((v (make-vector (length xs))))
    (let loop ((i 0) (rest xs))
      (if (null? rest) v
          (begin (vector-set! v i (car rest))
                 (loop (+ i 1) (cdr rest)))))))
(define (vector-fill! v x . range)
  (if (not (vector? v))
    (error 'wrong-type-arg "vector-fill!: expected a vector" v))
  (if (pair? range)
    (if (pair? (cdr range))
      (if (pair? (cddr range))
        (error 'wrong-number-of-args "vector-fill!: at most two indices"))))
  (let* ((len (vector-length v))
         (start (if (null? range) 0 (car range)))
         (end (if (or (null? range) (null? (cdr range))) len (cadr range))))
    (if (or (not (integer? start)) (not (integer? end)))
      (error 'wrong-type-arg "vector-fill!: expected integer indices" start end))
    (if (or (< start 0) (< end start) (> end len))
      (error 'out-of-range "vector-fill!: index out of range" start end))
    (let loop ((i start))
      (if (< i end)
        (begin (vector-set! v i x) (loop (+ i 1)))))))



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
