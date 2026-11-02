;; make-working-indicator
;; 构造工作状态动画指示器实例。
;;
;; 语法
;; ----
;; (make-working-indicator)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; working-indicator?
;; 新构造的工作指示器实例。

(import (scheme base)
  (liii check)
  (liii string)
  (liii unicode)
  (liii tui)
  (liii pi code modes interactive working)
) ;import

(check-set-mode! 'report)

;; Working 指示器帧序列与循环推进
(let ((ind (make-working-indicator)))
  (check-true (working-indicator? ind))
  ;; 初始帧为盲文点阵第一帧 ⠋
  (check (working-frame ind) => "⠋")
  ;; 推进一帧为 ⠙
  (working-advance! ind)
  (check (working-frame ind) => "⠙")
  ;; 再推进 9 帧共 10 帧，回到 ⠋
  (for-each (lambda (_) (working-advance! ind)) '(1 2 3 4 5 6 7 8 9))
  (check (working-frame ind) => "⠋")
) ;let

(check-report)
