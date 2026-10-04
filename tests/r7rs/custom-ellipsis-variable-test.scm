(import (scheme base) (liii check))
(check-set-mode! 'report-failed)
;; With ::: as ellipsis, ... is an ordinary pattern variable.
(define-syntax collect-two
  (syntax-rules ::: () ((_ x ...) (list x ...))))
(check (collect-two 1 2) => '(1 2))
(check-report)
