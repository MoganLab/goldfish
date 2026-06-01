(import (liii check) (liii sys))


(check-true (list? (argv)))



(check-true (string? (executable)))



(check-false (which "a-nonexistent-command-12345"))

(let ((gf-path (which "gf")))
  (when gf-path
    (check-true (string? gf-path))
  ) ;when
) ;let


(check-report)
