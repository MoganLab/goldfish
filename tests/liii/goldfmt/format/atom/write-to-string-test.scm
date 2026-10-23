(import (liii check))
(import (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; write-to-string
;; 使用 write 语义将值转为字符串，不受 *s7* 'print-length 限制。
;;
;; 语法
;; ----
;; (write-to-string value)
;;
;; 参数
;; ----
;; value : any?
;;
;; 返回值
;; ------
;; string?

;; 测试：字符串带引号写出
(check (write-to-string "abc") => "\"abc\"")

;; 测试：symbol 不带引号
(check (write-to-string 'abc) => "abc")

;; 测试：number
(check (write-to-string 42) => "42")

;; 测试：list 保留结构
(check (write-to-string '(1 "a")) => "(1 \"a\")")

(check-report)
