;; spinner-stop!
;; 停止微调器旋转并将状态置为停止（'stopped）。
;;
;; 语法
;; ----
;; (spinner-stop! obj)
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

(let ((s (make-spinner "Download" :frames 'dots)))
  (check (spinner-stop! s) => #t)
  (check (spinner-status s) => 'stopped)
  (check (spinner-current-frame s) => "■")
) ;let

(check-catch 'type-error (spinner-stop! "not-a-spinner"))

(check-report)
