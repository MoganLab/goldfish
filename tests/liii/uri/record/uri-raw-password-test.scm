;; uri-raw-password
;; 从 URI 的原始 netloc 字段读取密码。
;;
;; 语法
;; ----
;; (uri-raw-password uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回与 uri-password 相同的密码。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "alice:secret@example.com" "/" '() #f))
(check (uri-raw-password u) => "secret")
(check (uri-raw-password u) => (uri-password u))
(check (uri-raw-password (make-uri-raw "https" "alice@example.com" "/" '() #f))
  =>
  #f
) ;check

(check-report)
