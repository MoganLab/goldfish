(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-message
;; 读取错误消息。
;;
;; 语法
;; ----
;; (json-schema-error-message err)
;;
;; 参数
;; ----
;; err : json-schema-error
;; 错误对象。
;;
;; 返回值
;; ----
;; string
;;
;; 注意
;; ----
;; 消息是人类可读的说明文本。
;; 消息内容不作为程序判断依据，请使用错误类别与关键字。
;;
;; 示例
;; ----
;; (string? (json-schema-error-message (car (json-schema-errors 1 '(("type" . "integer"))))) => #t
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check-true (string? (json-schema-error-message err)))
(check-true (> (string-length (json-schema-error-message err)) 0))

(check-report)
