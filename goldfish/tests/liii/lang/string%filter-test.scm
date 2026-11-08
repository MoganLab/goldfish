;; string%filter
;; 保留字符串中满足谓词的字符（对标 Scala String.filter）。
;;
;; 语法
;; ----
;; ($ s :filter p)
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
;; string?
;; 仅含满足 (p c) 为真字符的新字符串，保持原序。

(import (liii check) (liii lang) (scheme char))

(check-set-mode! 'report-failed)

(check
  ($ "banana" :filter (lambda (c) (not (char=? c #\a))))
  =>
  "bnn"
) ;check
(check ($ "" :filter (lambda (c) #t)) => "")
(check ($ "a1b2c3" :filter char-numeric?) => "123")

(check-report)
