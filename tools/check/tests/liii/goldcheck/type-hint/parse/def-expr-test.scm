;; def-expr
;; 从 define 记录中提取对应的原始 S 表达式。
;;
;; 语法
;; ----
;; (def-expr d)
;;
;; 参数
;; ----
;; d : def  define 记录结构 (name kind arity line expr)
;;
;; 返回值
;; ------
;; pair
;; 原始 define S 表达式。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 提取表达式
(let* ((expr '(define (bar x) (+ x 1))) (d (list 'bar 'lambda 1 10 expr)))
  (check (def-expr d) => expr)
) ;let*

(check-report)
