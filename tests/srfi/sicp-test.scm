(import (srfi sicp)
  (liii os)
  (liii check)
) ;import

(display (runtime))
(newline)

(when (os-linux?)
  (os-call "sleep 0.01")
) ;when

(display (runtime))
(newline)

(check-true true)
(check-false false)
(check-true (null? nil))

(check-report)
