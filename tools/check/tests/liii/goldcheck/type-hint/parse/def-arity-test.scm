;; def-arity
;; 从 define 记录中提取参数数量规格（lambda 为整数，case-lambda 为列表，其他为 #f）。
;;
;; 语法
;; ----
;; (def-arity d)
;;
;; 参数
;; ----
;; d : def  define 记录结构 (name kind arity line expr)
;;
;; 返回值
;; ------
;; integer, list 或 #f
;; 参数数量规格。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. lambda 参数个数
(check (def-arity '(fn lambda 2 1 expr)) => 2)

;; 2. case-lambda 各子句参数个数
(check (def-arity '(cfn case-lambda (1 2) 2 expr)) => '(1 2))

;; 3. 非固定形参或值
(check (def-arity '(val value #f 3 expr)) => #f)

(check-report)
