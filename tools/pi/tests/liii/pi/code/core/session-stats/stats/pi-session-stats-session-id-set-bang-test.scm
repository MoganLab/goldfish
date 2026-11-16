;; pi-session-stats-session-id-set!
;; 更新 <pi-session-stats> 的 session-id 字段。
;;
;; 语法
;; ----
;; (pi-session-stats-session-id-set! stats id) -> unspecified

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
  (pi-session-stats-session-id-set! s "sid-1")
  (check (pi-session-stats-session-id s) => "sid-1")
) ;let

(check-report)
