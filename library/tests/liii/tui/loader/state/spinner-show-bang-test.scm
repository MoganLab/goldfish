;; spinner-show!
;; 显示微调器。
;;
;; 语法
;; ----
;; (spinner-show! obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; boolean?
;; 操作成功返回 #t。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Job" :visible #f)))
  (check-false (spinner-visible? s))
  (check (spinner-show! s) => #t)
  (check-true (spinner-visible? s))
) ;let

(check-catch 'type-error (spinner-show! "not-a-spinner"))

(check-report)
