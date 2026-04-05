(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-decode
;; 对百分比编码的字符串进行解码。
;;
;; 语法
;; ----
;; (uri-decode str)
;;
;; 返回值
;; ----
;; string?
;;   返回解码后的字符串。

;; 无需解码
(check (uri-decode "hello") => "hello")

;; 空格解码
(check (uri-decode "hello%20world") => "hello world")
(check (uri-decode "hello+world") => "hello world")

;; 特殊字符解码
(check (uri-decode "a%3Db") => "a=b")
(check (uri-decode "a%26b") => "a&b")

;; 错误处理
(check-catch 'type-error (uri-decode 123))

(check-report)
