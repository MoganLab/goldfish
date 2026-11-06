;; make-message-result
;; 构造带错误消息的失败解析结果。
;;
;; 语法
;; ----
;; (make-message-result pos msg)
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
;; <parse-result>
;; 失败的解析结果记录，其错误为 parse-error。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 3 15)))
  (let ((message (make-message-result pos "error")))
    (check-true (parse-result? message))
    (check-false (parse-result-successful? message))
    (check (parse-error-messages (parse-result-error message)) => '("error"))
  ) ;let
) ;let

(check-report)
