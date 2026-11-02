;; narrow-type
;; 根据假设（truthy 或具体类型谓词）对联合类型进行收窄（剔除 #f）。
;;
;; 语法
;; ----
;; (narrow-type t pred)
;;
;; 参数
;; ----
;; t : any       原始类型表达式（如 '(or #f string?)）
;; pred : symbol  收窄假设（'truthy 或具体类型谓词如 'string?）
;;
;; 返回值
;; ------
;; symbol 或 list
;; 收窄后的类型表达式。

(import (scheme base) (liii check) (liii goldcheck type-hint narrow))

(check-set-mode! 'report-failed)

;; 1. truthy 假设剔除 #f
(check (narrow-type '(or #f string?) 'truthy) => 'string?)

;; 2. 指定类型谓词收窄
(check (narrow-type '(or #f string? integer?) 'string?) => 'string?)

;; 3. 非联合类型或无 #f 原样返回
(check (narrow-type 'integer? 'truthy) => 'integer?)

(check-report)
