(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats-session-id
;; 返回统计中的会话标识。
;;
;; 语法
;; ----
;; (pi-session-stats-session-id stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; string 或 #f
;; 会话标识，缺失时为 #f。
;;
;; 示例
;; ----
;; (pi-session-stats-session-id stats) => "session-1"

(let ((stats (make-pi-session-stats #f "session-1" #f 0 0 0 0 0
               (make-pi-session-token-stats 0 0 0 0) 0)))
  (check (pi-session-stats-session-id stats) => "session-1"))

(check-report)
