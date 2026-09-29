;;; Hash-table compatibility layer used by SRFI-125. Tables use vectors of
;;; bucket alists; optional equivalence and hash procedures control lookup.

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
            (if (pair? spec) (cdr spec) #f)
            0)))

(define (hash-table? ht) (%s7-ht? ht))

(define (%s7-ht-equiv ht)
  (let ((equiv (vector-ref ht 2)))
    (if (procedure? equiv) equiv equal?)))

(define (%s7-ht-hash ht key)
  (let ((hash (vector-ref ht 3)))
    (if (procedure? hash) (hash key) (hash-code key))))

(define (%s7-ht-resize! ht new-size)
  (let* ((old-buckets (vector-ref ht 1))
         (new-buckets (make-vector new-size '())))
    (let bucket-loop ((i 0))
      (when (< i (vector-length old-buckets))
        (let cell-loop ((cells (vector-ref old-buckets i)))
          (when (pair? cells)
            (let ((cell (car cells)))
              (when (cdr cell)
                (let ((index (modulo (%s7-ht-hash ht (car cell)) new-size)))
                  (vector-set! new-buckets index
                               (cons cell (vector-ref new-buckets index)))))
            (cell-loop (cdr cells))))
        (bucket-loop (+ i 1))))
    (vector-set! ht 1 new-buckets))))

(define (%s7-ht-cell ht key)
  (unless (%s7-ht? ht)
    (error 'wrong-type-arg "expected a hash table" ht))
  (let* ((buckets (vector-ref ht 1))
         ;; modulo (floor), not remainder (truncate): a comparator's
         ;; hash may legally be negative (srfi-165 hashes variables by
         ;; their negative id), and a truncated bucket index reads
         ;; vector-ref at -1. Preserve the established table contract.
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
      (begin
        (when (and (cdr cell) (not value))
          (vector-set! ht 4 (- (vector-ref ht 4) 1)))
        (when (and (not (cdr cell)) value)
          (vector-set! ht 4 (+ (vector-ref ht 4) 1))
          (set-car! cell key))
        (set-cdr! cell value)
        value)
      (if (not value)
          #f
          (let* ((buckets (vector-ref ht 1))
                 (index (modulo (%s7-ht-hash ht key) (vector-length buckets)))
                 (count (+ (vector-ref ht 4) 1)))
            (vector-set! buckets index
                         (cons (cons key value) (vector-ref buckets index)))
            (vector-set! ht 4 count)
            (when (> count (* 2 (vector-length buckets)))
              (%s7-ht-resize! ht (* 2 (vector-length buckets))))
            value)))))

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
  (unless (%s7-ht? ht)
    (error 'wrong-type-arg "hash-table-size: expected a hash table" ht))
  (vector-ref ht 4))

(define (make-iterator ht)
  (unless (%s7-ht? ht)
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
              (cell-loop (cdr cells)
                (if (cdr (car cells))
                  (cons (car cells) acc)
                  acc)))))))))
