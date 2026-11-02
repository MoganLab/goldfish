;; type-ok?
;; 判定类型表达式是否符合类型系统规范（符号或合法的 or 联合类型）。
;;
;; 语法
;; ----
;; (type-ok? t)
;;
;; 参数
;; ----
;; t : any   待校验的类型表达式
;;
;; 返回值
;; ------
;; boolean
;; 类型表达式合法返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 单个符号类型合法
(check (type-ok? 'string?) => #t)
(check (type-ok? 'any?) => #t)

;; 2. 联合类型合法
(check (type-ok? '(or #f string?)) => #t)

;; 3. 非法类型表达式返回 #f
(check (type-ok? 123) => #f)
(check (type-ok? '(and a b)) => #f)

(check-report)
