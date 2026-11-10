;; single-arg-symbol-form?
;; 判断 value 是否为 (name x) 形式的单参数表单。
;;
;; 语法
;; ----
;; (single-arg-symbol-form? value name)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的 datum。
;;
;; name : symbol?
;; 期望的表单头符号。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 value 是以 name 为头且只有一个参数的表单。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (single-arg-symbol-form? (list 'quasiquote 'x) 'quasiquote) => #t)
(check (single-arg-symbol-form? (list 'unquote 'a 'b) 'unquote) => #f)
(check (single-arg-symbol-form? (list 'foo 'x) 'bar) => #f)
(check (single-arg-symbol-form? 'sym 'sym) => #f)

(check-report)
