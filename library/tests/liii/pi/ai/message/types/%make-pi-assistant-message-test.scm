;; %make-pi-assistant-message
;; 构造助手消息 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-assistant-message content stop-reason error-message usage timestamp provider model api)
;;
;; 参数
;; ----
;; content : any?
;; 助手回复正文，字符串或内容块列表
;;
;; stop-reason : string?
;; 结束原因，如 stop、tool-call、length
;;
;; error-message : string? 或 #f
;; 错误信息，成功时为 #f
;;
;; usage : alist? 或 #f
;; token 用量统计关联表
;;
;; timestamp : number?
;; 消息创建时间戳（毫秒）
;;
;; provider : string? 或 #f
;; 服务商标识
;;
;; model : string? 或 #f
;; 模型标识
;;
;; api : string? 或 #f
;; 协议接口标识
;;
;; 返回值
;; ----
;; pi-assistant-message?
;; 助手消息 record 实例。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-assistant-message "助手回复正文"
             "stop"
             #f
             '((input . 10) (output . 20))
             3000
             "google"
             "gemini-2.5-flash"
             "google-generative-ai"
           ) ;%make-pi-assistant-message
      ) ;obj
     ) ;
  (check-true (pi-assistant-message? obj))
  (check (pi-assistant-message-content obj) => "助手回复正文")
  (check (pi-assistant-message-stop-reason obj) => "stop")
  (check (pi-assistant-message-error-message obj) => #f)
  (check (pi-assistant-message-usage obj) => '((input . 10) (output . 20)))
  (check (pi-assistant-message-timestamp obj) => 3000)
  (check (pi-assistant-message-provider obj) => "google")
  (check (pi-assistant-message-model obj) => "gemini-2.5-flash")
  (check (pi-assistant-message-api obj) => "google-generative-ai")
) ;let

(check-report)
