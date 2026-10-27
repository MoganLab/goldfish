(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-path
;; 返回会话摘要中的 JSONL 文件路径。
;;
;; 语法
;; ----
;; (pi-session-info-path info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string
;; 会话文件路径。
;;
;; 示例
;; ----
;; (pi-session-info-path info) => "/tmp/session.jsonl"

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              #f "created" "modified" 0 "(no messages)" "")))
  (check (pi-session-info-path info) => "/tmp/session.jsonl"))

(check-report)
