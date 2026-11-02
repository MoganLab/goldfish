;; spinner-current-frame
;; 获取微调器当前应显示的动画帧或状态图标字符串。
;;
;; 语法
;; ----
;; (spinner-current-frame obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; string?
;; 当前帧字符串或终态图标。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Working" :frames 'line)))
  (check (spinner-current-frame s) => "-")
  (spinner-tick! s)
  (check (spinner-current-frame s) => "\\")
  (spinner-success! s)
  (check (spinner-current-frame s) => "✔")
) ;let

(check-catch 'type-error (spinner-current-frame "not-a-spinner"))

(check-report)
