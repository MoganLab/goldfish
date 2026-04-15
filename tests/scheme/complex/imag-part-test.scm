(import (liii check)
        (scheme complex)
) ;import

(check-set-mode! 'report-failed)

;; imag-part
;; 返回复数的虚部。
;;
;; 语法
;; ----
;; (imag-part z)
;;
;; 参数
;; ----
;; z : number 复数或实数。
;;
;; 返回值
;; ----
;; real 复数 z 的虚部。
;;
;; 描述
;; ----
;; `imag-part` 用于返回复数或实数的虚部。对于实数，返回 0；
;; 对于复数，返回其虚部。
;;
;; 数学定义
;; ------
;; 如果 z = x + yi，其中 x 和 y 是实数，i 是虚数单位，则：
;; imag-part(z) = y
;;
;; 示例
;; ----
;; (imag-part 5) => 0
;; (imag-part 3.14) => 0.0
;; (imag-part (make-rectangular 3 4)) => 4
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是数值类型时抛出错误。

;; Test imag-part with complex numbers
(check (imag-part (make-rectangular 3 4)) => 4.0)
(check (imag-part (make-rectangular -3 4)) => 4.0)
(check (imag-part (make-rectangular 3 -4)) => -4.0)
(check (imag-part (make-rectangular -3 -4)) => -4.0)

;; Test imag-part with real numbers
(check (imag-part 5) => 0)
(check (imag-part -5) => 0)
(check (imag-part 0) => 0)

;; Test imag-part with floating point numbers
(check (imag-part 3.14) => 0.0)
(check (imag-part -2.71) => 0.0)

;; Test imag-part with complex number literals
(check (imag-part 1+2i) => 2.0)
(check (imag-part 3-4i) => -4.0)
(check (imag-part -5+6i) => 6.0)
(check (imag-part -7-8i) => -8.0)
(check (imag-part 0+9i) => 9.0)
(check (imag-part 10+0i) => 0.0)

(check-report)
