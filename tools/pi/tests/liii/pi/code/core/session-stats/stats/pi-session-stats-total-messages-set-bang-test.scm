;; pi-session-stats-total-messages-set!
;; 更新 <pi-session-stats> 的 total-messages 字段。
;;
;; 语法
;; ----
;; (pi-session-stats-total-messages-set! stats n) -> unspecified

;;
;; 参数
;; ----
;; stats : pi-session-stats?
;; val : any
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base) (liii check) (liii pi code core session-stats stats))

(check-set-mode! 'report-failed)

(let ((s (make-pi-session-stats "a.jsonl")))
  (pi-session-stats-total-messages-set! s 5)
  (check (pi-session-stats-total-messages s) => 5)
) ;let

(check-report)
