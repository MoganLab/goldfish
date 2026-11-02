;; def-kind
;; 从 define 记录中提取定义类别（lambda / case-lambda / rest / value）。
;;
;; 语法
;; ----
;; (def-kind d)
;;
;; 参数
;; ----
;; d : def  define 记录结构 (name kind arity line expr)
;;
;; 返回值
;; ------
;; symbol
;; 定义类别符号。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 提取各类定义类别
(check (def-kind '(fn lambda 1 1 expr)) => 'lambda)
(check (def-kind '(cfn case-lambda (1 2) 2 expr)) => 'case-lambda)
(check (def-kind '(val value #f 3 expr)) => 'value)

(check-report)
