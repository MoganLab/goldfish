;; string%forall
;; 判断字符串是否全部字符满足谓词（对标 Scala String.forall）。
;;
;; 语法
;; ----
;; ($ s :forall p)
;;
;; 参数
;; ----
;; p : procedure?
;; 字符谓词；可用 `_` 占位符简写。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; boolean
;; 所有字符满足 (p c) 为真则 #t；空串为 #t（空真）。UTF-8 语义。

(import (liii check) (liii lang) (scheme char))

(check-set-mode! 'report-failed)

(check ($ "abc" :forall (lambda (c) (char<? c #\d))) => #t)
(check ($ "abc" :forall (lambda (c) (char<? c #\c))) => #f)
(check ($ "" :forall (lambda (c) #t)) => #t)

(check-report)
