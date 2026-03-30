(import (liii check)
        (scheme complex)
) ;import

(check-set-mode! 'report-failed)

;; make-rectangular
;; 按直角坐标构造复数。
;;
;; 语法
;; ----
;; (make-rectangular real imag)
;;
;; 参数
;; ----
;; real : real 复数实部。
;; imag : real 复数虚部。
;;
;; 返回值
;; ----
;; number 若 imag 为 0，可能返回实数；否则返回复数。
;;
;; 描述
;; ----
;; `make-rectangular` 按实部和虚部构造复数，在 (scheme complex) 中与 `complex` 语义一致。
;;
;; 示例
;; ----
;; (make-rectangular 3 0) => 3
;; (make-rectangular 2.5 0.0) => 2.5
;; (make-rectangular 3 4) 的实部为 3，虚部为 4
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是实数类型时抛出错误。
;; wrong-number-of-args 当参数个数错误时抛出错误。

;; Test make-rectangular
(check (make-rectangular 3 0) => 3)
(check (make-rectangular 2.5 0.0) => 2.5)
(check (real-part (make-rectangular 3 4)) => 3.0)
(check (imag-part (make-rectangular 3 4)) => 4.0)

;; Error handling
(check-catch 'wrong-type-arg (make-rectangular "x" 1))
(check-catch 'wrong-type-arg (make-rectangular 1 "x"))
(check-catch 'wrong-number-of-args (make-rectangular 1))

(check-report)
