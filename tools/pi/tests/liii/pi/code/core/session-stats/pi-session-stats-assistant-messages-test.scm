(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats-assistant-messages
;; 返回助手消息数量。
;;
;; 语法
;; ----
;; (pi-session-stats-assistant-messages stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; integer
;; role 为 assistant 的消息数量。
;;
;; 示例
;; ----
;; (pi-session-stats-assistant-messages stats) => 2

(let ((stats (make-pi-session-stats #f #f #f 2 0 2 0 0
               (make-pi-session-token-stats 0 0 0 0) 0)))
  (check (pi-session-stats-assistant-messages stats) => 2))

(check-report)
