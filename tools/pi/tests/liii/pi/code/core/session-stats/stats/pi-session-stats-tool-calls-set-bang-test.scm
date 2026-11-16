;; pi-session-stats-tool-calls-set!
;; 更新 <pi-session-stats> 的 tool-calls 字段。
;;
;; 语法
;; ----
;; (pi-session-stats-tool-calls-set! stats n) -> unspecified

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
  (pi-session-stats-tool-calls-set! s 4)
  (check (pi-session-stats-tool-calls s) => 4)
) ;let

(check-report)
