;; micro-call.scm -- call-heavy benchmark for the VM-vs-eval decision.
;; Pure recursion: exercises the bytecode loop + inline primitive fast
;; paths on the VM side, and the plain s7 evaluator on the other side.

(import (scheme base))

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(fib 26)
