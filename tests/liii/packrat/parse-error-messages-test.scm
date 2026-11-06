;; parse-error-messages
;; 读取解析错误中的消息列表。
;;
;; 语法
;; ----
;; (parse-error-messages err)
;;
;; 参数
;; ----
;; err : <parse-error>
;; 解析错误记录。
;;
;; 返回值
;; ----
;; list
;; 错误消息的字符串列表；没有消息时为空列表。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 2 10)))
  (check (parse-error-messages (make-error-message pos "syntax error"))
    =>
    '("syntax error")
  ) ;check
  (check (parse-error-messages (make-error-expected pos "open-paren")) => '())
) ;let

(check-report)
