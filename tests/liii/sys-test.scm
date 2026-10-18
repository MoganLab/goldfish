(import (liii check) (liii sys))


(check-true (list? (argv)))



(check-true (string? (executable)))



(check-false (which "a-nonexistent-command-12345"))

(let ((gf-path (which "gf")))
  (when gf-path
    (check-true (string? gf-path))
  ) ;when
) ;let

(check-true (boolean? (stdin-tty?)))
(check-true (boolean? (stdout-tty?)))
(check-true (boolean? (stderr-tty?)))

(check-catch 'type-error (sleep 'not-a-number))

(check-report)
