;; error-object-message
;; 返回错误对象的消息字符串。
;;
;;
;; 语法
;; ----
;; (error-object-message error-object)
;;
;; 参数
;; ----
;; error-object : error-object?
;; 待提取消息的错误对象。
;;
;; 返回值
;; ----
;; string?
;; 错误对象中包含的描述消息。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(define err1 (guard (ex (else ex)) (error "something went wrong" 'a 'b)))
(define err2 (guard (ex (else ex)) (error "cannot open file" "path/to/file")))

(check (error-object-message err1) => "something went wrong")
(check (error-object-message err2) => "cannot open file")

(check-report)
