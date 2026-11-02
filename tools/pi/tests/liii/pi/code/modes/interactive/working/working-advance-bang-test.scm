;; working-advance!
;; 将工作指示器推进到下一帧（循环滚动）。
;;
;; 语法
;; ----
;; (working-advance! ind)
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

;; 1. 推进一帧下标自增
(let ((ind (make-working-indicator)))
  (working-advance! ind)
  (check (working-frame-index ind) => 1)
  (check (working-frame ind) => "⠙")
) ;let

(check-report)
