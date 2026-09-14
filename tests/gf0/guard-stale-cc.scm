(import (scheme base) (srfi srfi-158))

;; Fence probe (see tools/check-gf0-guards.sh): each call caught, key printed.
;; s7 side: 10, 20, eof. gf0 side: fence is fail-closed (the user callback is
;; a gf0 box, so even the first yield nests a fresh token) -> stale key x3.
(define (show thunk)
  (display (catch #t thunk (lambda args (car args))))
  (newline)
) ;define

(let ((g (make-coroutine-generator (lambda (yield) (yield 10) (yield 20)))))
  (show g)
  (show g)
  (show g)
) ;let
