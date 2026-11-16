;; pi-session-stats-session-name-set!
;; 更新 <pi-session-stats> 的 session-name 字段。
;;
;; 语法
;; ----
;; (pi-session-stats-session-name-set! stats name) -> unspecified

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
  (pi-session-stats-session-name-set! s "My Session")
  (check (pi-session-stats-session-name s) => "My Session")
) ;let

(check-report)
