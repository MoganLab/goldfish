;; uri-password
;; 从 URI 的网络位置解析并读取密码。
;;
;; 语法
;; ----
;; (uri-password uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回密码；未提供密码时返回 #f。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define with-password
  (make-uri-raw "https" "alice:secret@example.com" "/" '() #f)
) ;define

(define empty-password (make-uri-raw "https" "alice:@example.com" "/" '() #f))

(define without-password (make-uri-raw "https" "alice@example.com" "/" '() #f))
(check (uri-password with-password) => "secret")
(check (uri-password empty-password) => "")
(check (uri-password without-password) => #f)

(check-report)
