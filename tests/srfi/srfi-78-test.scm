
(import (srfi srfi-78))


(display "----------\n")
(display "check mode: report\n")

(check-set-mode! 'report)

(check (+ 1 2) => 3)

(check (+ 1 2) => 4)

(check-reset!)

(display "\n----------\n")
(display "check mode: off\n")

(check-set-mode! 'off)

(check (+ 1 2) => 3)

(check (+ 1 2) => 4)

(check-reset!)

(display "\n----------\n")
(display "check mode: report-failed\n")

(check-set-mode! 'report-failed)

(check (+ 1 2) => 3)

(check (+ 1 2) => 4)

(check-reset!)

(display "\n----------\n")
(display "check mode: summary\n")

(check-set-mode! 'summary)

(check (+ 1 2) => 3)

(check (+ 1 2) => 4)

(check-report)

(check-set-mode! 'report)

(check-report)
