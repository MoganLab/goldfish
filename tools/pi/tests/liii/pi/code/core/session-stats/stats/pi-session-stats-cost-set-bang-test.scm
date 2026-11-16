;; pi-session-stats-cost-set!
;; 更新 <pi-session-stats> 的 cost 字段。
;;
;; 语法
;; ----
;; (pi-session-stats-cost-set! stats cost) -> unspecified

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
  (pi-session-stats-cost-set! s 0.25)
  (check (pi-session-stats-cost s) => 0.25)
) ;let

(check-report)
