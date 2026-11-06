;; let-form?
;; 判断某个 tag 是否为 let 类表单。
;;
;; 语法
;; ----
;; (let-form? tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;; env 的 tag-name。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示该 tag 属于 let 类表单。
;;
;; 说明
;; ----
;; 包含 let、let*、letrec、letrec*、let-values 与 let*-values。

(import (liii check) (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; 测试：let 类表单返回 #t
(check (let-form? "let") => #t)
(check (let-form? "let*") => #t)
(check (let-form? "letrec") => #t)
(check (let-form? "letrec*") => #t)
(check (let-form? "let-values") => #t)
(check (let-form? "let*-values") => #t)

;; 测试：其他 tag 返回 #f
(check (let-form? "define") => #f)
(check (let-form? "lambda") => #f)
(check (let-form? "") => #f)

(check-report)
