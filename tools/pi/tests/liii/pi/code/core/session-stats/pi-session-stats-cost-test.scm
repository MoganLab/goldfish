(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats-cost
;; 返回会话累计费用。
;;
;; 语法
;; ----
;; (pi-session-stats-cost stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; number
;; 所有助手消息费用的总和。
;;
;; 示例
;; ----
;; (pi-session-stats-cost stats) => 0.5

(let ((paid (make-pi-session-stats #f #f #f 0 0 0 0 0
              (make-pi-session-token-stats 0 0 0 0) 0.5))
      (free (make-pi-session-stats #f #f #f 0 0 0 0 0
              (make-pi-session-token-stats 0 0 0 0) 0)))
  (check (pi-session-stats-cost paid) => 0.5)
  (check (pi-session-stats-cost free) => 0))

(check-report)
