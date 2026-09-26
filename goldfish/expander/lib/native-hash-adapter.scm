;;; native-hash-adapter.scm -- the s7 hashtable surface for the native host.
;;;
;;; Loaded ONLY by the native driver (src/runtime/native_main.cpp), right
;;; after base-functions.scm.  The host gets hash-table?, hash-table-ref,
;;; hash-table-size, s7-make-hash-table, s7-hash-table-set! and
;;; make-iterator from s7 itself; the native substrate has no table object,
;;; so this file provides the same observable contract on top of a vector
;;; of bucket alists:
;;;
;;;   - a stored #f reads back as "absent" -- srfi-125's
;;;     hash-table-delete! / hash-table-clear! implement deletion by
;;;     storing #f and hash-table-contains? tests (not (not ref));
;;;   - hash-table-size counts stored cells (the tests only pin
;;;     fresh-empty and set-then-nonempty);
;;;   - make-iterator yields (key . value) pairs and then eof forever;
;;;   - the optional (equiv . hash) pair from s7-make-hash-table drives
;;;     lookup and bucketing; without it, equal? and hash-code apply.

(define (%s7-ht? ht)
  (and (vector? ht)
       (> (vector-length ht) 0)
       (eq? (vector-ref ht 0) 's7-hash-table)))

(define (s7-make-hash-table . opts)
  (let ((size (if (and (pair? opts) (integer? (car opts)) (> (car opts) 0))
                (car opts)
                16))
        (spec (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) #f)))
    (vector 's7-hash-table
            (make-vector size '())
            (if (pair? spec) (car spec) #f)
            (if (pair? spec) (cdr spec) #f))))

(define (hash-table? ht) (%s7-ht? ht))

(define (%s7-ht-equiv ht)
  (let ((equiv (vector-ref ht 2)))
    (if (procedure? equiv) equiv equal?)))

(define (%s7-ht-hash ht key)
  (let ((hash (vector-ref ht 3)))
    (if (procedure? hash) (hash key) (hash-code key))))

(define (%s7-ht-cell ht key)
  (if (not (%s7-ht? ht))
    (error 'wrong-type-arg "expected a hash table" ht))
  (let* ((buckets (vector-ref ht 1))
         ;; modulo (floor), not remainder (truncate): a comparator's
         ;; hash may legally be negative (srfi-165 hashes variables by
         ;; their negative id), and a truncated bucket index reads
         ;; vector-ref at -1.  Matches the host's s7 table behaviour.
         (bucket (vector-ref buckets
                   (modulo (%s7-ht-hash ht key) (vector-length buckets)))))
    (let loop ((cells bucket))
      (if (null? cells)
        #f
        (if ((%s7-ht-equiv ht) key (caar cells))
          (car cells)
          (loop (cdr cells)))))))

(define (s7-hash-table-set! ht key value)
  (let ((cell (%s7-ht-cell ht key)))
    (if cell
      (begin (set-cdr! cell value) value)
      (let* ((buckets (vector-ref ht 1))
             (index (modulo (%s7-ht-hash ht key) (vector-length buckets))))
        (vector-set! buckets index
                     (cons (cons key value) (vector-ref buckets index)))
        value))))

(define (s7-hash-table-ref ht key . maybe-default)
  (let ((cell (%s7-ht-cell ht key)))
    (if cell
      (cdr cell)
      (if (pair? maybe-default)
        (let ((default (car maybe-default)))
          (if (procedure? default) (default) default))
        #f))))

(define (hash-table-ref ht key) (s7-hash-table-ref ht key))

(define (hash-table-size ht)
  (if (not (%s7-ht? ht))
    (error 'wrong-type-arg "hash-table-size: expected a hash table" ht))
  (let ((buckets (vector-ref ht 1)))
    (let bucket-loop ((i (- (vector-length buckets) 1)) (total 0))
      (if (< i 0)
        total
        (bucket-loop (- i 1)
          (let cell-loop ((cells (vector-ref buckets i)) (count total))
            (if (null? cells)
              count
              (cell-loop (cdr cells) (+ count 1)))))))))

(define (make-iterator ht)
  (if (not (%s7-ht? ht))
    (error 'wrong-type-arg "make-iterator: expected a hash table" ht))
  (let ((buckets (vector-ref ht 1)))
    (let collect ((i (- (vector-length buckets) 1)) (entries '()))
      (if (< i 0)
        (let walk ((rest entries))
          (lambda ()
            (if (pair? rest)
              (let ((entry (car rest)))
                (set! rest (cdr rest))
                entry)
              (eof-object))))
        (collect (- i 1)
          (let cell-loop ((cells (vector-ref buckets i)) (acc entries))
            (if (null? cells)
              acc
              (cell-loop (cdr cells) (cons (car cells) acc)))))))))
