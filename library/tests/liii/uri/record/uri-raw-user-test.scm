;; uri-raw-user
;; 从 URI 的原始 netloc 字段读取用户名。
;;
;; 语法
;; ----
;; (uri-raw-user uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回与 uri-user 相同的用户名。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "alice:secret@example.com" "/" '() #f))
(check (uri-raw-user u) => "alice")
(check (uri-raw-user u) => (uri-user u))
(check (uri-raw-user (make-uri-raw "https" "example.com" "/" '() #f)) => #f)

(check-report)
