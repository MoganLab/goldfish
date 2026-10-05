(import (goldfish) (liii check))

(check-set-mode! 'report-failed)

(define (entries ht)
  (let ((next (make-iterator ht)))
    (let loop ((result '()))
      (let ((entry (next)))
        (if (eof-object? entry) result
            (loop (cons entry result)))))))

(define (fill! ht start end)
  (let loop ((key start))
    (when (< key end)
      (s7-hash-table-set! ht key (+ key 1))
      (loop (+ key 1)))))

;; Crossing the first threshold must visit each live entry exactly once.
(let* ((calls 0)
       (ht (s7-make-hash-table 2
             (cons = (lambda (key) (set! calls (+ calls 1)) key)))))
  (fill! ht 0 4)
  (set! calls 0)
  (fill! ht 4 5)
  (check calls => 6)
  (check (hash-table-size ht) => 5)
  (check (length (entries ht)) => 5)
  (check (map (lambda (key) (s7-hash-table-ref ht key)) '(0 1 2 3 4))
         => '(1 2 3 4 5)))

;; Exercise several growths with distributed, colliding and negative hashes.
(for-each
  (lambda (hash)
    (let ((ht (s7-make-hash-table 2 (cons = hash))))
      (fill! ht 0 80)
      (s7-hash-table-set! ht 0 999)
      (s7-hash-table-set! ht 1 #f)
      (s7-hash-table-set! ht 2 #f)
      (fill! ht 80 160)
      (check (hash-table-size ht) => 158)
      (check (length (entries ht)) => 158)
      (check (s7-hash-table-ref ht 0) => 999)
      (check (s7-hash-table-ref ht 1) => #f)
      (check (s7-hash-table-ref ht 2) => #f)
      (check (let loop ((key 3))
               (or (= key 160)
                   (and (= (s7-hash-table-ref ht key) (+ key 1))
                        (loop (+ key 1))))) => #t)
      (s7-hash-table-set! ht 1 2)
      (check (hash-table-size ht) => 159)
      (check (length (entries ht)) => 159)
      (check (s7-hash-table-ref ht 1) => 2)))
  (list (lambda (key) key) (lambda (key) 0) (lambda (key) (- key))))

(check-report)
(if (check-failed?) (exit 1))
