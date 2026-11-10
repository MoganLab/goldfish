;; uri-path-set!
;; 原地修改 URI 记录的 path 字段。
;;
;; 语法
;; ----
;; (uri-path-set! uri-obj value)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要修改的 URI 记录。
;; value : string? 或 #f
;;   新的路径值。
;;
;; 返回值
;; ----
;; unspecified
;;   修改记录后的返回值未指定，应通过访问器检查结果。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/old" '() #f))
(uri-path-set! u "/new")
(check (uri-path-raw u) => "/new")
(check (uri-host u) => "example.com")
(uri-path-set! u "")
(check (uri-path-raw u) => "")

(check-report)
