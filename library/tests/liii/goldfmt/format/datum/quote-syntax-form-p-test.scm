;; quote-syntax-form?
;; 判断 value 是否为 #_quote 语法对象包裹的单参数表单。
;;
;; 语法
;; ----
;; (quote-syntax-form? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的 datum。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 value 是 quote 语法形式。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(let ((q (read (open-input-string "'x"))))
  (check (quote-syntax-form? q) => #t)
) ;let
(check (quote-syntax-form? '(a b)) => #f)

(check-report)
