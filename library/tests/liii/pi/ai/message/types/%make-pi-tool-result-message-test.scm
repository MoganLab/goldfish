;; %make-pi-tool-result-message
;; 构造工具执行结果消息 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-tool-result-message tool-call-id tool-name content is-error timestamp)
;;
;; 参数
;; ----
;; tool-call-id : string?
;; 对应的工具调用标识
;;
;; tool-name : string?
;; 工具名称
;;
;; content : any?
;; 工具输出内容
;;
;; is-error : boolean?
;; 是否为错误结果
;;
;; timestamp : number?
;; 消息创建时间戳（毫秒）
;;
;; 返回值
;; ----
;; pi-tool-result-message?
;; 工具执行结果消息 record 实例。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-tool-result-message "call-001" "read" "文件内容" #f 5000)))
  (check-true (pi-tool-result-message? obj))
  (check (pi-tool-result-message-tool-call-id obj) => "call-001")
  (check (pi-tool-result-message-tool-name obj) => "read")
  (check (pi-tool-result-message-content obj) => "文件内容")
  (check (pi-tool-result-message-is-error obj) => #f)
  (check (pi-tool-result-message-timestamp obj) => 5000)
) ;let

(check-report)
