(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats-total-messages
;; 返回会话消息总条数。
;;
;; 语法
;; ----
;; (pi-session-stats-total-messages stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; integer
;; 包括普通消息与 bashExecution 的消息总数。
;;
;; 示例
;; ----
;; (pi-session-stats-total-messages stats) => 5

(define (stats-with-total n)
  (make-pi-session-stats #f #f #f n 0 0 0 0
    (make-pi-session-token-stats 0 0 0 0) 0))

(check (pi-session-stats-total-messages (stats-with-total 5)) => 5)
(check (pi-session-stats-total-messages (stats-with-total 0)) => 0)

(check-report)
