;; %accepts?
;; 判断实参类型能否作为期望类型传入。
;;
;; 语法
;; ----
;; (%accepts? expected actual)
;;
;; 参数
;; ----
;; expected : symbol | list | #f
;; 期望类型：类型符号、联合类型 (or T ...) 或 any?。
;;
;; actual : symbol | list | #f
;; 实参推导类型；#f 表示未知（无法确定）。
;;
;; 返回值
;; ------
;; boolean
;; 实参类型可传给期望类型时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii goldcheck type-hint compat)
) ;import

(check-set-mode! 'report-failed)

;; 1. 相同类型与 any?
(check (%accepts? 'string? 'string?) => #t)
(check (%accepts? 'any? 'string?) => #t)
(check (%accepts? 'string? 'integer?) => #f)

;; 2. number 层次：integer? 可传 number?；同组类型保守视为不冲突
(check (%accepts? 'number? 'integer?) => #t)
(check (%accepts? 'integer? 'number?) => #t)

;; 3. 未知类型（自定义谓词）不判定冲突
(check (%accepts? 'alist? 'string?) => #t)

;; 4. 实参未知时不报冲突
(check (%accepts? 'string? #f) => #t)

;; 5. 期望类型为联合类型：实际类型至少要被其中一个成员接受
(check (%accepts? '(or #f string?) 'string?) => #t)
(check (%accepts? '(or #f string?) 'integer?) => #f)
(check (%accepts? '(or #f string? integer?) 'integer?) => #t)

(check-report)
