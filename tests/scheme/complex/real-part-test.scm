(import (liii check)
        (scheme complex)
) ;import

(check-set-mode! 'report-failed)

;; real-part
;; 返回复数的实部。
;;
;; 语法
;; ----
;; (real-part z)
;;
;; 参数
;; ----
;; z : number 复数或实数。
;;
;; 返回值
;; ----
;; real 复数 z 的实部。
;;
;; 描述
;; ----
;; `real-part` 用于返回复数或实数的实部。对于实数，返回该实数本身；
;; 对于复数，返回其实部。
;;
;; 数学定义
;; ------
;; 如果 z = x + yi，其中 x 和 y 是实数，i 是虚数单位，则：
;; real-part(z) = x
;;
;; 示例
;; ----
;; (real-part 5) => 5
;; (real-part 3.14) => 3.14
;; (real-part (make-rectangular 3 4)) => 3
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是数值类型时抛出错误。

;; Test real-part with complex numbers
(check (real-part (make-rectangular 3 4)) => 3.0)
(check (real-part (make-rectangular -3 4)) => -3.0)
(check (real-part (make-rectangular 3 -4)) => 3.0)
(check (real-part (make-rectangular -3 -4)) => -3.0)

;; Test real-part with real numbers
(check (real-part 5) => 5)
(check (real-part -5) => -5)
(check (real-part 0) => 0)

;; Test real-part with floating point numbers
(check (real-part 3.14) => 3.14)
(check (real-part -2.71) => -2.71)

;; Test real-part with complex number literals
(check (real-part 1+2i) => 1.0)
(check (real-part 3-4i) => 3.0)
(check (real-part -5+6i) => -5.0)
(check (real-part -7-8i) => -7.0)
(check (real-part 0+9i) => 0.0)
(check (real-part 10+0i) => 10.0)

(check-report)
