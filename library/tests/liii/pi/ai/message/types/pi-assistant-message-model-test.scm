;; pi-assistant-message-model
;; 获取助手消息 record 的 model 字段。
;;
;; 语法
;; ----
;; (pi-assistant-message-model obj)
;;
;; 参数
;; ----
;; obj : any?
;; 助手消息 record 实例。
;;
;; 返回值
;; ----
;; string? 或 #f
;; model 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-assistant-message "助手回复正文"
                "stop"
                #f
                '((input . 10) (output . 20))
                3000
                "google"
                "gemini-2.5-flash"
                "google-generative-ai"
              ) ;%make-pi-assistant-message
       ) ;obj-a
       (obj-b (%make-pi-assistant-message "助手回复正文" "stop" #f #f 4000 #f #f #f)
       ) ;obj-b
      ) ;
  (check (pi-assistant-message-model obj-a) => "gemini-2.5-flash")
  (check (pi-assistant-message-model obj-b) => #f)
) ;let*

(check-report)
