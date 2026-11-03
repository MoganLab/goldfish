;; pi-assistant-message-stop-reason
;; 获取助手消息 record 的 stop-reason 字段。
;;
;; 语法
;; ----
;; (pi-assistant-message-stop-reason obj)
;;
;; 参数
;; ----
;; obj : any?
;; 助手消息 record 实例。
;;
;; 返回值
;; ----
;; string?
;; stop-reason 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-assistant-message "助手回复正文"
                "stop"
                #f
                '((input . 10) (output . 20))
                3000
              ) ;%make-pi-assistant-message
       ) ;obj-a
       (obj-b (%make-pi-assistant-message '("块一" "块二")
                "tool-call"
                "upstream error"
                #f
                4000
              ) ;%make-pi-assistant-message
       ) ;obj-b
      ) ;
  (check (pi-assistant-message-stop-reason obj-a) => "stop")
  (check (pi-assistant-message-stop-reason obj-b) => "tool-call")
) ;let*

(check-report)
