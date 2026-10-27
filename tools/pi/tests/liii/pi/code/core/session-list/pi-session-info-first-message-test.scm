(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-first-message
;; 返回首条用户消息的投影文本。
;;
;; 语法
;; ----
;; (pi-session-info-first-message info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string
;; 首条用户消息文本，无消息时为占位文本。
;;
;; 示例
;; ----
;; (pi-session-info-first-message info) => "first question"

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              #f "created" "modified" 1 "first question" "first question answer")))
  (check (pi-session-info-first-message info) => "first question"))

(check-report)
