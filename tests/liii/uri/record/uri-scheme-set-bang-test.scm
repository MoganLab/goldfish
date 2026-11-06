;; uri-scheme-set!
;; 原地修改 URI 记录的 scheme 字段。
;;
;; 语法
;; ----
;; (uri-scheme-set! uri-obj value)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要修改的 URI 记录。
;; value : string? 或 #f
;;   新的 scheme 值。
;;
;; 返回值
;; ----
;; unspecified
;;   修改记录后的返回值未指定，应通过访问器检查结果。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "http" "example.com" "/" '() #f))
(uri-scheme-set! u "https")
(check (uri-scheme-raw u) => "https")
(check (uri-netloc-raw u) => "example.com")
(uri-scheme-set! u #f)
(check (uri-scheme-raw u) => #f)

(check-report)
