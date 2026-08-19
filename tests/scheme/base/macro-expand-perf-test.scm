(import (scheme base))
(import (liii timeit))
(import (goldfish))

;; 性能基准：syntax-rules 宏展开开销
;; 基线（预编译前）：my-or x2000 ~= 10.7s
;; 目标：预编译 syntax-rules 模板（template）后显著下降。

(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ e) e)
    ((_ e1 e2 ...)
     (let ((t e1))
       (if t t (my-or e2 ...))))))

;; A strict program resolves identifiers only from its imports, and my-or is
;; a macro of THIS program -- so expand-eval (the session program library)
;; is the right evaluator, not a fresh (environment '(scheme base)).
(define (expand-my-or)
  (expand-eval '(my-or #f #f #f #f #f #f #f #f #f #f
                       #f #f #f #f #f #f #f #f #f #t)))

(display "my-or x2000: ")
(display (timeit expand-my-or '() 2000))
(newline)
