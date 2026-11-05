;; pi-tool-result-message
;; 构造 toolResult 角色消息 record 对象。
;;
;; 语法
;; ----
;; (pi-tool-result-message tool-call-id tool-name content is-error)
;; (pi-tool-result-message tool-call-id tool-name content is-error timestamp)
;;
;; 参数
;; ----
;; tool-call-id : string?
;; 工具调用标识符。
;;
;; tool-name : string?
;; 工具名称。
;;
;; content : any?
;; 工具执行结果内容。
;;
;; is-error : boolean?
;; 是否属于执行错误。
;;
;; timestamp : number?
;; 可选，时间戳。
;;
;; 返回值
;; ----
;; pi-tool-result-message?
;; 工具执行结果消息 record。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(let* ((res-content (list (pi-text-block "file contents here")))
       (msg (pi-tool-result-message "call_123" "read" res-content #f))
      ) ;
  (check-true (pi-tool-result-message? msg))
  (check-true (pi-message? msg))
  (check (pi-message-role msg) => "toolResult")
  (check (pi-tool-result-message-tool-call-id msg) => "call_123")
  (check (pi-tool-result-message-tool-name msg) => "read")
  (check (pi-tool-result-message-content msg) => res-content)
  (check (pi-tool-result-message-is-error msg) => #f)
  (check-true (number? (pi-tool-result-message-timestamp msg)))

  (let ((err-msg (pi-tool-result-message "call_456" "bash" "failed" #t 99999)))
    (check (pi-tool-result-message-is-error err-msg) => #t)
    (check (pi-tool-result-message-timestamp err-msg) => 99999)
  ) ;let
) ;let*

(check-report)
