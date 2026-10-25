(import (liii check) (liii sys))


(check-true (list? (argv)))



(check-true (string? (executable)))



(check-true (boolean? (stdin-tty?)))
(check-true (boolean? (stdout-tty?)))
(check-true (boolean? (stderr-tty?)))

(check-report)
