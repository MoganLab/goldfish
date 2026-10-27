(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-modified
;; 返回会话最近活动时间戳。
;;
;; 语法
;; ----
;; (pi-session-info-modified info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string
;; 最近一条消息的时间戳，无消息时为创建时间戳。
;;
;; 示例
;; ----
;; (pi-session-info-modified info) => "2026-10-20T08:00:02.000Z"

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              #f "created" "2026-10-20T08:00:02.000Z" 0 "(no messages)" "")))
  (check (pi-session-info-modified info) => "2026-10-20T08:00:02.000Z"))

(check-report)
