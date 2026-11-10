;; pi-make-tool-result-message
;; 构造工具执行结果消息快捷方式。
;;
;; 语法
;; ----
;; (pi-make-tool-result-message tool-call-id tool-name content is-error)
;;
;; 参数
;; ----
;; tool-call-id : string?
;; 对应的工具调用 ID。
;;
;; tool-name : string?
;; 工具名称。
;;
;; content : any?
;; 工具执行结果内容。
;;
;; is-error : boolean?
;; 是否执行失败。
;;
;; 返回值
;; ----
;; pi-tool-result-message?
;; 工具结果消息 record 实例。

(import (liii check) (liii pi ai types))

(check-set-mode! 'report-failed)

(let ((msg (pi-make-tool-result-message "call_1" "read" "file content" #f)))
  (check-true (pi-tool-result-message? msg))
  (check (pi-tool-result-message-role msg) => "toolResult")
  (check (pi-tool-result-message-tool-call-id msg) => "call_1")
  (check (pi-tool-result-message-tool-name msg) => "read")
  (check-false (pi-tool-result-message-is-error msg))
) ;let

(check-report)
