;; pi-session-stats-user-messages-set!
;; 更新 <pi-session-stats> 的 user-messages 字段。
;;
;; 语法
;; ----
;; (pi-session-stats-user-messages-set! stats n) -> unspecified

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
  (pi-session-stats-user-messages-set! s 2)
  (check (pi-session-stats-user-messages s) => 2)
) ;let

(check-report)
