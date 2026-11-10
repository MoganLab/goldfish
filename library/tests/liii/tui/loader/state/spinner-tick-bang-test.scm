;; spinner-tick!
;; 推进微调器动画帧索引并返回当前帧字符。
;;
;; 语法
;; ----
;; (spinner-tick! obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; string?
;; 推进后的当前帧或终态图标。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

;; line 帧推进与循环模运算
(let ((s (make-spinner "Working" :frames 'line)))
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "-")
  ;; tick 1 -> "\"
  (check (spinner-tick! s) => "\\")
  (check (spinner-frame-index s) => 1)
  (check (spinner-current-frame s) => "\\")
  ;; tick 2 -> "|"
  (check (spinner-tick! s) => "|")
  (check (spinner-frame-index s) => 2)
  ;; tick 3 -> "/"
  (check (spinner-tick! s) => "/")
  (check (spinner-frame-index s) => 3)
  ;; tick 4 -> 循环回到 "-" (index 0)
  (check (spinner-tick! s) => "-")
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "-")
) ;let

;; 终态下 tick! 不推进帧
(let ((s (make-spinner "Build")))
  (spinner-success! s)
  (check (spinner-tick! s) => "✔")
) ;let

(check-catch 'type-error (spinner-tick! "not-a-spinner"))

(check-report)
