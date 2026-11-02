;; working-last-tick-set!
;; 设置工作指示器上次动画节拍的时间戳。
;;
;; 语法
;; ----
;; (working-last-tick-set! ind val)
;;
;; 参数
;; ----
;; ind : working-indicator?
;; 工作指示器实例。
;; val : integer? or boolean?
;; 新的时间戳值。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base) (liii check) (liii pi code modes interactive working))

(check-set-mode! 'report-failed)

;; 1. 设置并读取时间戳
(let ((ind (make-working-indicator)))
  (working-last-tick-set! ind 12345)
  (check (working-last-tick ind) => 12345)
) ;let

(check-report)
