;;; base-functions.scm -- library functions implemented in Scheme.
;;;
;;; Loaded into the implementation library after the module system starts.
;;; All code resolves `map' and `for-each' through this Scheme implementation.
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
;; Reject invalid arity, types and ranges instead of silently truncating.
(define (negative? x)
  (unless (and (number? x) (real? x))
    (error 'wrong-type-arg "negative? expects a real number" x))
  (< x 0))
(define (boolean=? x y . rest)
  (let same? ((objs (cons y rest)) (first x))
    (if (null? objs)
      #t
      (and (boolean? first) (boolean? (car objs)) (eq? first (car objs))
           (same? (cdr objs) first)))))
(define (odd? x)
  (unless (exact-integer? x)
    (error 'wrong-type-arg "odd?: expected an integer" x))
  (not (= (modulo x 2) 0)))
(define (even? x)
  (unless (exact-integer? x)
    (error 'wrong-type-arg "even?: expected an integer" x))
  (= (modulo x 2) 0))
(define (abs x) (if (negative? x) (- 0 x) x))
(define (make-list n . fill)
  (unless (integer? n)
    (error 'wrong-type-arg "make-list: expected an integer length" n))
  (when (and (pair? fill) (pair? (cdr fill)))
    (error 'wrong-number-of-args "make-list: at most one fill value"))
  (when (< n 0)
    (error 'out-of-range "make-list: length must be non-negative" n))
  (let loop ((i n) (out '()))
    (if (= i 0)
      (reverse out)
      (loop (- i 1) (cons (if (null? fill) #f (car fill)) out)))))
(define (list-copy x)
  (if (pair? x) (cons (car x) (list-copy (cdr x))) x))
(define (list-tail x n)
  (unless (or (pair? x) (null? x))
    (error 'wrong-type-arg "list-tail: expected a list" x))
  (unless (integer? n)
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
  (unless (vector? v)
    (error 'wrong-type-arg "vector->list: expected a vector" v))
  (when (pair? range)
    (when (pair? (cdr range))
      (when (pair? (cddr range))
        (error 'wrong-number-of-args "vector->list: at most two indices"))))
  (let* ((len (vector-length v))
         (start (if (null? range) 0 (car range)))
         (end (if (or (null? range) (null? (cdr range))) len (cadr range))))
    (unless (and (integer? start) (integer? end))
      (error 'wrong-type-arg "vector->list: expected integer indices" start end))
    (when (or (< start 0) (< end start) (> end len))
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
;; R7RS vector-copy: a fresh vector holding v[start, end).
(define (vector-copy . args)
  (when (null? args)
    (error 'wrong-type-arg "vector-copy: expected a vector"))
  (let ((v (car args)) (range (cdr args)))
    (unless (vector? v)
      (error 'wrong-type-arg "vector-copy: expected a vector" v))
    (when (pair? range)
      (when (pair? (cdr range))
        (when (pair? (cddr range))
          (error 'wrong-number-of-args "vector-copy: at most two indices"))))
    (let* ((len (vector-length v))
           (start (if (null? range) 0 (car range)))
           (end (if (or (null? range) (null? (cdr range))) len (cadr range))))
      (unless (and (integer? start) (integer? end))
        (error 'wrong-type-arg "vector-copy: expected integer indices" start end))
      (when (or (< start 0) (< end start) (> end len))
        (error 'out-of-range "vector-copy: index out of range" start end))
      (let ((result (make-vector (- end start))))
        (let fill ((i start))
          (when (< i end)
            (vector-set! result (- i start) (vector-ref v i))
            (fill (+ i 1)))
      result)))))
(define (vector-fill! v x . range)
  (unless (vector? v)
    (error 'wrong-type-arg "vector-fill!: expected a vector" v))
  (when (pair? range)
    (when (pair? (cdr range))
      (when (pair? (cddr range))
        (error 'wrong-number-of-args "vector-fill!: at most two indices"))))
  (let* ((len (vector-length v))
         (start (if (null? range) 0 (car range)))
         (end (if (or (null? range) (null? (cdr range))) len (cadr range))))
    (unless (and (integer? start) (integer? end))
      (error 'wrong-type-arg "vector-fill!: expected integer indices" start end))
    (when (or (< start 0) (< end start) (> end len))
      (error 'out-of-range "vector-fill!: index out of range" start end))
    (let loop ((i start))
      (when (< i end)
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
      (unless (null? l)
        (f (car l))
        (fe1 (cdr l))))
    (let fen ((l1 l1) (rest rest))
      (when (and (not (null? l1)) (lists-live? rest))
        (apply f (car l1) (map-cars rest))
        (fen (cdr l1) (map-cdrs rest))))))


;; Fold follows the conventional (accumulator element) calling order used by
;; the expander's set helpers and SRFI-1.

;; R7RS vector-append: a fresh vector concatenating every argument.
(define (vector-append . vecs)
  (for-each (lambda (v)
              (unless (vector? v)
                (error 'wrong-type-arg "vector-append: expected vectors" v)))
            vecs)
  (let ((total (let loop ((rest vecs) (n 0))
                 (if (null? rest)
                   n
                   (loop (cdr rest) (+ n (vector-length (car rest))))))))
    (let build ((rest vecs) (index 0) (out (make-vector total)))
      (if (null? rest)
        out
        (let ((v (car rest)))
          (let copy-i ((i 0))
            (if (< i (vector-length v))
              (begin (vector-set! out (+ index i) (vector-ref v i))
                     (copy-i (+ i 1)))
              (build (cdr rest) (+ index (vector-length v)) out))))))))

;; R7RS vector-copy!: copy from[start, end) into to starting at `at'.  The
;; source range is snapshotted first, so self-copies of one vector behave
;; like memmove.  A missing source argument reports wrong-type-arg, which
;; is what the contract's two-argument case expects.
(define (vector-copy! to at . rest)
  (unless (vector? to)
    (error 'wrong-type-arg "vector-copy!: expected a vector" to))
  (unless (integer? at)
    (error 'wrong-type-arg "vector-copy!: expected an integer index" at))
  (let ((from (if (pair? rest) (car rest) '()))
        (range (if (pair? rest) (cdr rest) '())))
    (unless (vector? from)
      (error 'wrong-type-arg "vector-copy!: expected a vector" from))
    (when (pair? range)
      (when (pair? (cdr range))
        (when (pair? (cddr range))
          (error 'wrong-number-of-args "vector-copy!: at most two indices"))))
    (let* ((flen (vector-length from))
           (start (if (null? range) 0 (car range)))
           (end (if (or (null? range) (null? (cdr range))) flen (cadr range))))
      (unless (and (integer? start) (integer? end))
        (error 'wrong-type-arg "vector-copy!: expected integer indices" start end))
      (when (or (< start 0) (< end start) (> end flen))
        (error 'out-of-range "vector-copy!: index out of range" start end))
      (when (or (< at 0) (> (+ at (- end start)) (vector-length to)))
        (error 'out-of-range "vector-copy!: index out of range" at))
      (let snapshot ((i start) (items '()))
        (if (>= i end)
          (let write ((index at) (pending (reverse items)))
            (if (null? pending)
              to
              (begin (vector-set! to index (car pending))
                     (write (+ index 1) (cdr pending)))))
          (snapshot (+ i 1) (cons (vector-ref from i) items)))))))
