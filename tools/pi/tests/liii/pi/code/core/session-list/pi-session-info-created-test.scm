(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-created
;; 返回会话创建时间戳。
;;
;; 语法
;; ----
;; (pi-session-info-created info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string
;; 会话 header 中记录的创建时间戳。
;;
;; 示例
;; ----
;; (pi-session-info-created info) => "2026-10-20T08:00:00.000Z"

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              #f "2026-10-20T08:00:00.000Z" "modified" 0 "(no messages)" "")))
  (check (pi-session-info-created info) => "2026-10-20T08:00:00.000Z"))

(check-report)
