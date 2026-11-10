;; spinner-interval
;; 获取微调器的建议刷新间隔（毫秒）。
;;
;; 语法
;; ----
;; (spinner-interval obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; integer?
;; 刷新间隔（毫秒）。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Job" :interval 120)))
  (check (spinner-interval s) => 120)
) ;let

(check-catch 'type-error (spinner-interval "not-a-spinner"))

(check-report)
