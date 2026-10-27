(import (scheme base)
  (liii check)
  (liii string)
  (liii unicode)
  (liii tui)
  (liii pi code modes interactive working))

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
  (check (working-frame ind) => "⠋"))

(check-report)
