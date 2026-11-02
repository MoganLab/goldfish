;; working-reset!
;; 将工作指示器复位到初始帧与初始时间戳状态。
;;
;; 语法
;; ----
;; (working-reset! ind)
;;
;; 参数
;; ----
;; ind : working-indicator?
;; 工作指示器实例。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base) (liii check) (liii pi code modes interactive working))

(check-set-mode! 'report-failed)

;; 1. 推进数帧后复位回到初始帧与 #f
(let ((ind (make-working-indicator)))
  (working-advance! ind)
  (working-advance! ind)
  (working-last-tick-set! ind 1000)
  (working-reset! ind)
  (check (working-frame-index ind) => 0)
  (check (working-last-tick ind) => #f)
) ;let

(check-report)
