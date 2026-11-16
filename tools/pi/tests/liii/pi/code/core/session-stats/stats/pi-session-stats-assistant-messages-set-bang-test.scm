;; pi-session-stats-assistant-messages-set!
;; 更新 <pi-session-stats> 的 assistant-messages 字段。
;;
;; 语法
;; ----
;; (pi-session-stats-assistant-messages-set! stats n) -> unspecified

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
  (pi-session-stats-assistant-messages-set! s 3)
  (check (pi-session-stats-assistant-messages s) => 3)
) ;let

(check-report)
