(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; check-float
;; 使用绝对误差判断两个数值是否近似相等。
;;
;; 语法
;; ----
;; (check-float a b [epsilon])
;;
;; 参数
;; ----
;; a : number?
;; b : number?
;; epsilon : number?
;; 可选的绝对误差容差，默认值为 `1e-10`。

(check-true (check-float (+ 0.1 0.2) 0.3))
(check-true (check-float (+ 0.1 0.2) 0.3 1e-12))
(check-false (check-float (+ 0.1 0.2) 0.3001 1e-6))

(check-report)
