;; spinner-frame-index
;; 获取微调器当前帧的索引（从 0 开始）。
;;
;; 语法
;; ----
;; (spinner-frame-index obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; integer?
;; 当前帧索引。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Working" :frames 'line)))
  (check (spinner-frame-index s) => 0)
  (spinner-tick! s)
  (check (spinner-frame-index s) => 1)
) ;let

(check-catch 'type-error (spinner-frame-index "not-a-spinner"))

(check-report)
