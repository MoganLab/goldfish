(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats-user-messages
;; 返回用户消息数量。
;;
;; 语法
;; ----
;; (pi-session-stats-user-messages stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; integer
;; role 为 user 的消息数量。
;;
;; 示例
;; ----
;; (pi-session-stats-user-messages stats) => 1

(let ((stats (make-pi-session-stats #f #f #f 1 1 0 0 0
               (make-pi-session-token-stats 0 0 0 0) 0)))
  (check (pi-session-stats-user-messages stats) => 1))

(check-report)
