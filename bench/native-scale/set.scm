(define-library (native-scale set)
  (export run-set-workload run-set-phases)
  (import (scheme base) (scheme process-context) (liii set) (native-scale timing))
  (begin
    (define (range count)
      (let loop ((i 0) (xs '()))
        (if (= i count) (reverse xs) (loop (+ i 1) (cons i xs)))))

    ;; Match tests/liii/set/set-size-test.scm: retain the first list and set
    ;; while constructing a second, almost-as-large list and set.
    (define (million-set-workload measure)
      (let ((n (string->number (get-environment-variable "GOLDFISH_BENCH_SIZE"))))
        (unless (and (integer? n) (exact? n) (> n 0))
          (error "invalid benchmark size"))
        (let* ((big-list (measure 'list-build-big (lambda () (range n))))
               (big-set (measure 'set-build-big (lambda () (list->set big-list))))
               (small-list (measure 'list-build-small (lambda () (range (- n 1)))))
               (small-set (measure 'set-build-small (lambda () (list->set small-list)))))
          (measure 'check
            (lambda ()
              (unless (and (= (set-size big-set) n)
                           (= (set-size small-set) (- n 1)))
                (error "benchmark set cardinality check failed"))
              (unless (and (set-contains? big-set (- n 1))
                           (not (set-contains? big-set n))
                           (set-contains? small-set (- n 2))
                           (not (set-contains? small-set (- n 1))))
                (error "benchmark set membership check failed"))))
          'BENCH-OK)))

    (define (run-set-workload)
      (million-set-workload (lambda (label thunk) (thunk))))
    (define (run-set-phases) (million-set-workload phase-measure))))
