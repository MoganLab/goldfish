;; def-name
;; 从 define 记录中提取符号名称。
;;
;; 语法
;; ----
;; (def-name d)
;;
;; 参数
;; ----
;; d : def  define 记录结构 (name kind arity line expr)
;;
;; 返回值
;; ------
;; symbol
;; 定义的函数或变量名称符号。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 提取名称
(let ((d '(foo lambda 1 5 (define (foo x) x))))
  (check (def-name d) => 'foo)
) ;let

(check-report)
