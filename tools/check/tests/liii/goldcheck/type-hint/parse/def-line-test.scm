;; def-line
;; 从 define 记录中提取定义所在源码行号。
;;
;; 语法
;; ----
;; (def-line d)
;;
;; 参数
;; ----
;; d : def  define 记录结构 (name kind arity line expr)
;;
;; 返回值
;; ------
;; integer
;; 源码行号。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 提取行号
(let ((d '(foo lambda 1 88 (define (foo x) x))))
  (check (def-line d) => 88)
) ;let

(check-report)
