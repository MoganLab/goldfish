;; working-indicator?
;; 判定对象是否为工作状态指示器实例。
;;
;; 语法
;; ----
;; (working-indicator? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive working))

(check-set-mode! 'report-failed)

;; 1. make-working-indicator 返回实例判定为 #t
(check-true (working-indicator? (make-working-indicator)))

;; 2. 非指示器对象判定为 #f
(check-false (working-indicator? #f))
(check-false (working-indicator? 123))
(check-false (working-indicator? "indicator"))

(check-report)
