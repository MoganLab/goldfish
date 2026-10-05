(import (goldfish))
(define table (s7-make-hash-table 2 (cons = (lambda (key) key)) (cons #t #t)))
(for-each (lambda (key) (s7-hash-table-set! table key key)) '(0 1 2 3 4))
(list 'reported-size (hash-table-size table)
      'bucket-count (vector-length (vector-ref table 1))
      'stored-entries
      (let loop ((i 0) (n 0))
        (if (= i (vector-length (vector-ref table 1))) n
            (loop (+ i 1) (+ n (length (vector-ref (vector-ref table 1) i)))))))
