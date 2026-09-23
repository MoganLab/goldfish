(import (liii check) (srfi srfi-48))

(check-set-mode! 'report-failed)

;; SRFI-48: Intermediate Format Strings
;; (format [port] format-string . args)

(check (format "Hello, ~a!" "World") => "Hello, World!")
(check (format "~a + ~a = ~a" 1 2 3) => "1 + 2 = 3")
(check (format #f "pi = ~f" 3.14159) => "pi = 3.141590")

(check-report)
