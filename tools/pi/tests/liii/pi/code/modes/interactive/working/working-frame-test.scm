;; working-frame
;; 获取工作指示器当前动画帧的盲文字符串。
;;
;; 语法
;; ----
;; (working-frame ind)
;;
;; 参数
;; ----
;; ind : working-indicator?
;; 工作指示器实例。
;;
;; 返回值
;; ----
;; string?
;; 当前帧的字符表示。

(import (scheme base) (liii check) (liii pi code modes interactive working))

(check-set-mode! 'report-failed)

;; 1. 初始帧为第一帧
(let ((ind (make-working-indicator)))
  (check (working-frame ind) => "⠋")
) ;let

(check-report)
