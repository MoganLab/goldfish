(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-all-messages-text
;; 返回用于会话搜索的全部消息投影文本。
;;
;; 语法
;; ----
;; (pi-session-info-all-messages-text info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string
;; 用户和助手消息按顺序拼接的搜索文本。
;;
;; 示例
;; ----
;; (pi-session-info-all-messages-text info) => "first question first answer"

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              #f "created" "modified" 2 "first question"
              "first question first answer")))
  (check (pi-session-info-all-messages-text info) => "first question first answer"))

(check-report)
