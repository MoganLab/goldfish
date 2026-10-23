(import (liii check))
(import (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; string-contains-newline?
;; 判断文本是否包含换行符。
;;
;; 语法
;; ----
;; (string-contains-newline? text)
;;
;; 参数
;; ----
;; text : string?
;;
;; 返回值
;; ------
;; boolean?

;; 测试：包含换行返回 #t
(check (string-contains-newline? "a\nb") => #t)

;; 测试：不含换行返回 #f
(check (string-contains-newline? "abc") => #f)

;; 测试：空字符串返回 #f
(check (string-contains-newline? "") => #f)

(check-report)
