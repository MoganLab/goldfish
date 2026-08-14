(import (scheme base))
(import (scheme eval))
(import (liii timeit))

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

(define (expand-my-or)
  (eval '(my-or #f #f #f #f #f #f #f #f #f #f
                #f #f #f #f #f #f #f #f #f #t)
        (environment '(scheme base))))

(display "my-or x2000: ")
(display (timeit expand-my-or '() 2000))
(newline)
