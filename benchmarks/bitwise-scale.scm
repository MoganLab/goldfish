;; Run: ./bin/gf benchmarks/bitwise-scale.scm [bits]
;; Monotonic timings exclude bootstrap and compilation; checksums validate each run.
(import (only (goldfish) g_monotonic-nanosecond) (liii bitwise) (scheme base) (scheme write)
        (scheme process-context))

(define width
  (let ((argument (string->number (car (reverse (command-line))))))
    (if (and argument (exact-integer? argument) (positive? argument)) argument 4096)))
(define rounds 16)
(define high (arithmetic-shift 1 width))
(define dense (- high 1))

(define (measure label proc expected)
  (unless (= (proc) expected) (error "benchmark preflight failed" label))
  (do ((sample 0 (+ sample 1))) ((= sample 3))
    (let ((start (g_monotonic-nanosecond)))
      (let loop ((remaining rounds) (checksum 0))
        (if (zero? remaining)
          (let ((elapsed (- (g_monotonic-nanosecond) start)))
            (unless (= checksum (* expected rounds)) (error "benchmark checksum failed" label))
            (write (list label 'bits width 'rounds rounds 'elapsed-ns elapsed 'checksum checksum))
            (newline))
          (loop (- remaining 1) (+ checksum (proc))))))))

(measure 'dense-bit-count (lambda () (bit-count dense)) width)
(measure 'sparse-bit-count (lambda () (bit-count high)) 1)
(measure 'positive-integer-length (lambda () (integer-length high)) (+ width 1))
(measure 'negative-integer-length (lambda () (integer-length (- high))) width)

(let ((start (g_monotonic-nanosecond)))
  (let loop ((remaining 200000) (count 0))
    (if (zero? remaining)
      (begin
        (unless (= count 200000) (error "tail-loop checksum failed"))
        (write (list 'tail-loop 'iterations count
                     'elapsed-ns (- (g_monotonic-nanosecond) start)))
        (newline))
      (loop (- remaining 1) (+ count 1)))))
