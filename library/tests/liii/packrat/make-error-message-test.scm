;; make-error-message
;; 构造带错误消息的解析错误。
;;
;; 语法
;; ----
;; (make-error-message pos msg)
;;
;; 参数
;; ----
;; pos : <parse-position> 或 #f
;; 出错位置。
;; msg : string
;; 错误消息。
;;
;; 返回值
;; ----
;; <parse-error>
;; 解析错误记录，其 messages 字段为单元素列表。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 2 10)))
  (let ((error-msg (make-error-message pos "syntax error")))
    (check-true (parse-error? error-msg))
    (check (parse-error-position error-msg) => pos)
    (check (parse-error-messages error-msg) => '("syntax error"))
    (check (parse-error-expected error-msg) => '())
  ) ;let
) ;let

(check-report)
