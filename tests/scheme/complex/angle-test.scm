(import (liii check)
        (scheme complex)
) ;import

(check-set-mode! 'report-failed)

;; angle
;; 返回复数的辐角。
;;
;; 语法
;; ----
;; (angle z)
;;
;; 参数
;; ----
;; z : number 复数或实数。
;;
;; 返回值
;; ----
;; real z 的主辐角（弧度）。
;;
;; 描述
;; ----
;; `angle` 返回复数在复平面中的方向角；对实数按符号返回 0 或 π。
;;
;; 数学定义
;; ------
;; 如果 z = x + yi，则 angle(z) = atan2(y, x)。
;;
;; 示例
;; ----
;; (angle 1) => 0
;; (angle -1) => π
;; (angle 1+1i) ≈ 0.785398...
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是数值类型时抛出错误。

;; Test angle with real numbers
(check (angle 1) => 0)
(check (angle -1) => 3.141592653589793)

;; Test angle with complex numbers
(check (> (angle 1+1i) 0.78) => #t)
(check (< (angle 1+1i) 0.79) => #t)

;; Error handling
(check-catch 'wrong-type-arg (angle "x"))

(check-report)
