;; uri-user
;; 从 URI 的网络位置解析并读取用户名。
;;
;; 语法
;; ----
;; (uri-user uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回用户名，不含密码；未提供用户名时返回 #f。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define with-password
  (make-uri-raw "https" "alice:secret@example.com" "/" '() #f)
) ;define

(define without-password (make-uri-raw "https" "alice@example.com" "/" '() #f))

(define anonymous (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-user with-password) => "alice")
(check (uri-user without-password) => "alice")
(check (uri-user anonymous) => #f)

(check-report)
