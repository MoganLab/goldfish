(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

(define (mark m) (display m) (newline) (flush-output-port))

(mark "M0-start")

(define ch1 (make-chan))
(mark "M1-make-chan-0")
(check (chan? ch1) => #t)
(mark "M2-chan-pred")

(define ch2 (make-chan 20))
(mark "M3-make-chan-20")
(check (chan? ch2) => #t)
(mark "M4-chan-pred2")

(check-catch 'type-error (make-chan -1))
(mark "M5-neg-cap")

(check-catch 'type-error (make-chan "bad"))
(mark "M6-str-cap")

(check-report)
(mark "M7-end")
