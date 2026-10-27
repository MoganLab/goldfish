(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats-session-file
;; 返回统计来源的会话文件路径。
;;
;; 语法
;; ----
;; (pi-session-stats-session-file stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; string 或 #f
;; 文件会话返回路径，内存会话返回 #f。
;;
;; 示例
;; ----
;; (pi-session-stats-session-file stats) => "/tmp/session.jsonl"

(let ((file-stats (make-pi-session-stats "/tmp/session.jsonl" #f #f 0 0 0 0 0
                    (make-pi-session-token-stats 0 0 0 0) 0))
      (memory-stats (make-pi-session-stats #f #f #f 0 0 0 0 0
                      (make-pi-session-token-stats 0 0 0 0) 0)))
  (check (pi-session-stats-session-file file-stats) => "/tmp/session.jsonl")
  (check-false (pi-session-stats-session-file memory-stats)))

(check-report)
