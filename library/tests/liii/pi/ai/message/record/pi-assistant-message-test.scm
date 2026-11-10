;; pi-assistant-message
;; 构造 assistant 角色消息 record 对象。
;;
;; 语法
;; ----
;; (pi-assistant-message content)
;; (pi-assistant-message content :stop-reason stop-reason :error-message err :usage usage :timestamp ts)
;;
;; 参数
;; ----
;; content : any?
;; 消息内容（块列表或字符串）。
;;
;; 返回值
;; ----
;; pi-assistant-message?
;; assistant 消息 record 对象。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(let* ((content (list (pi-text-block "thinking...")))
       (msg (pi-assistant-message content
              :stop-reason   "stop"
              :error-message #f
              :usage         '((input . 10) (output . 20) (total . 30))
            ) ;pi-assistant-message
       ) ;msg
      ) ;
  (check-true (pi-assistant-message? msg))
  (check-true (pi-message? msg))
  (check (pi-message-role msg) => "assistant")
  (check (pi-assistant-message-content msg) => content)
  (check (pi-assistant-message-stop-reason msg) => "stop")
  (check (pi-assistant-message-error-message msg) => #f)
  (check (pi-assistant-message-usage msg)
    =>
    '((input . 10) (output . 20) (total . 30))
  ) ;check
  (check-true (number? (pi-assistant-message-timestamp msg)))

  (let ((default-msg (pi-assistant-message "simple text")))
    (check (pi-assistant-message-content default-msg) => "simple text")
    (check (pi-assistant-message-stop-reason default-msg) => "stop")
    (check (pi-assistant-message-error-message default-msg) => #f)
  ) ;let
) ;let*

(check-report)
