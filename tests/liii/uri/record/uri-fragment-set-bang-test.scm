;; uri-fragment-set!
;; 原地修改 URI 记录的 fragment 字段。
;;
;; 语法
;; ----
;; (uri-fragment-set! uri-obj value)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要修改的 URI 记录。
;; value : string? 或 #f
;;   新的片段标识符。
;;
;; 返回值
;; ----
;; unspecified
;;   修改记录后的返回值未指定，应通过访问器检查结果。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/" '() #f))
(uri-fragment-set! u "section")
(check (uri-fragment-raw u) => "section")
(check (uri-path-raw u) => "/")
(uri-fragment-set! u #f)
(check (uri-fragment-raw u) => #f)

(check-report)
