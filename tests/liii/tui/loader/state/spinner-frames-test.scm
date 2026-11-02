;; spinner-frames
;; 获取微调器的动画帧字符串列表。
;;
;; 语法
;; ----
;; (spinner-frames obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; list?
;; 动画帧字符串列表。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Working" :frames 'line)))
  (check (spinner-frames s) => '("-" "\\" "|" "/"))
) ;let

(let ((s (make-spinner "Dots" :frames 'dots)))
  (check (car (spinner-frames s)) => "⠋")
) ;let

(check-catch 'type-error (spinner-frames "not-a-spinner"))

(check-report)
