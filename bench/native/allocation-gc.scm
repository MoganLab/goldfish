;; Native allocation/collector baseline. GOLDFISH_DEBUG=gc adds the BDWGC
;; collection statistics to stderr; this file keeps deterministic results
;; and repeated monotonic timing samples on stdout.

(import (scheme base) (scheme time))

(define allocation-count 10000)
(define payload-size 256)

(define (allocate-round)
  (let loop ((i allocation-count) (total 0))
    (if (= i 0)
        total
        (let* ((text (make-string payload-size #\x))
               (bytes (string->utf8 text)))
          (loop (- i 1) (+ total (bytevector-length bytes)))))))

(define expected-result (* allocation-count payload-size))
(if (not (= (allocate-round) expected-result))
    (error "allocation benchmark result mismatch"))

(display "allocation round: strings=")
(display allocation-count)
(display " bytes-per-string=")
(display payload-size)
(display " result=")
(display expected-result)
(newline)

(do ((sample 0 (+ sample 1))) ((= sample 5))
  (let* ((start (monotonic-nanosecond))
         (result (allocate-round))
         (elapsed (- (monotonic-nanosecond) start)))
    (if (not (= result expected-result))
        (error "allocation benchmark result mismatch"))
    (display "  ns=")
    (display elapsed)
    (display " result=")
    (display result)
    (newline)))
