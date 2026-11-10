;; pi-assistant-message-content
;; 获取助手消息 record 的 content 字段。
;;
;; 语法
;; ----
;; (pi-assistant-message-content obj)
;;
;; 参数
;; ----
;; obj : any?
;; 助手消息 record 实例。
;;
;; 返回值
;; ----
;; any?
;; content 字段的值。

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
  (check (pi-assistant-message-content obj-a) => "助手回复正文")
  (check (pi-assistant-message-content obj-b) => '("块一" "块二"))
) ;let*

(check-report)
