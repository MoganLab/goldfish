(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-id
;; 返回会话摘要中的会话标识。
;;
;; 语法
;; ----
;; (pi-session-info-id info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string
;; 会话标识。
;;
;; 示例
;; ----
;; (pi-session-info-id info) => "session-1"

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              #f "created" "modified" 0 "(no messages)" "")))
  (check (pi-session-info-id info) => "session-1"))

(check-report)
