;; working-frame-index
;; 获取工作指示器当前的帧下标（0..9）。
;;
;; 语法
;; ----
;; (working-frame-index ind)
;;
;; 参数
;; ----
;; ind : working-indicator?
;; 工作指示器实例。
;;
;; 返回值
;; ----
;; integer?
;; 当前帧下标。

(import (scheme base) (liii check) (liii pi code modes interactive working))

(check-set-mode! 'report-failed)

;; 1. 初始下标为 0
(let ((ind (make-working-indicator)))
  (check (working-frame-index ind) => 0)
) ;let

(check-report)
