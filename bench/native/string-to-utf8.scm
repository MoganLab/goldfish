;; Native-only string->utf8 benchmark. Report raw monotonic nanoseconds so
;; it remains useful while the native numeric tower has no inexact values.

(import (scheme base) (scheme time))

(define (run-case label text iterations)
  (let ((sink 0))
    ;; Warm instruction/data paths before the timed samples.
    (do ((i 0 (+ i 1))) ((= i 100))
      (set! sink (+ sink (bytevector-length (string->utf8 text)))))
    (display label)
    (newline)
    (do ((sample 0 (+ sample 1))) ((= sample 5))
      (let ((start (monotonic-nanosecond)))
        (do ((i 0 (+ i 1))) ((= i iterations))
          (set! sink (+ sink (bytevector-length (string->utf8 text)))))
        (let ((elapsed (- (monotonic-nanosecond) start)))
          (display "  ns=")
          (display elapsed)
          (display " ns/op=")
          (display (quotient elapsed iterations))
          (newline))))
    sink))

(run-case "ASCII 16 bytes x 20000" "0123456789abcdef" 20000)
(run-case "CJK 1024 chars x 1000" (make-string 1024 #\中) 1000)
(run-case "ASCII 1024 bytes x 1000" (make-string 1024 #\a) 1000)
