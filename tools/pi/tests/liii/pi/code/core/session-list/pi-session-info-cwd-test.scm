(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-cwd
;; 返回会话记录的工作目录。
;;
;; 语法
;; ----
;; (pi-session-info-cwd info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string
;; 工作目录路径。
;;
;; 示例
;; ----
;; (pi-session-info-cwd info) => "/tmp/project"

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              #f "created" "modified" 0 "(no messages)" "")))
  (check (pi-session-info-cwd info) => "/tmp/project"))

(check-report)
