;; working-last-tick
;; 获取工作指示器上次动画节拍的时间戳。
;;
;; 语法
;; ----
;; (working-last-tick ind)
;;
;; 参数
;; ----
;; ind : working-indicator?
;; 工作指示器实例。
;;
;; 返回值
;; ----
;; integer? or boolean?
;; 上次节拍时间戳；未开始时返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive working))

(check-set-mode! 'report-failed)

;; 1. 初始为 #f
(let ((ind (make-working-indicator)))
  (check (working-last-tick ind) => #f)
) ;let

(check-report)
