(import (liii check))
(import (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; no-keyword-args-form?
;; 判断表单 tag 是否不携带 keyword 参数（define、import 等定义类表单）。
;;
;; 语法
;; ----
;; (no-keyword-args-form? tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;;
;; 返回值
;; ------
;; boolean?

;; 测试：define 类表单为 #t
(check (no-keyword-args-form? "define") => #t)
(check (no-keyword-args-form? "define-values") => #t)
(check (no-keyword-args-form? "define-record-type") => #t)

;; 测试：import/export 为 #t
(check (no-keyword-args-form? "import") => #t)
(check (no-keyword-args-form? "export") => #t)

;; 测试：普通表单为 #f
(check (no-keyword-args-form? "list") => #f)
(check (no-keyword-args-form? "make") => #f)

(check-report)
