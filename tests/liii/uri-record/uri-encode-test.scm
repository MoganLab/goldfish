(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-encode
;; 对字符串进行百分比编码。
;;
;; 语法
;; ----
;; (uri-encode str)
;;
;; 返回值
;; ----
;; string?
;;   返回编码后的字符串。

;; 无需编码
(check (uri-encode "hello") => "hello")
(check (uri-encode "test-file.txt") => "test-file.txt")

;; 空格编码
(check (uri-encode "hello world") => "hello%20world")

;; 特殊字符编码
(check (uri-encode "a=b") => "a%3Db")
(check (uri-encode "a&b") => "a%26b")

;; 中文字符编码（如果支持）
;; (check (uri-encode "中文") => "%E4%B8%AD%E6%96%87")

;; 错误处理
(check-catch 'type-error (uri-encode 123))
(check-catch 'type-error (uri-encode '()))

(check-report)
