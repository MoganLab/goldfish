;; spinner-reset!
;; 重置微调器状态回旋转中（'spinning）并将帧索引复位为 0。
;;
;; 语法
;; ----
;; (spinner-reset! obj)
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

(let ((s (make-spinner "Run" :frames 'line)))
  (spinner-success! s "Done")
  (check (spinner-status s) => 'success)
  (check (spinner-reset! s) => #t)
  (check (spinner-status s) => 'spinning)
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "-")
) ;let

(check-catch 'type-error (spinner-reset! "not-a-spinner"))

(check-report)
