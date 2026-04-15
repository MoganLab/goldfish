(import (liii check) (scheme complex))
(check-set-mode! 'report-failed)
;; magnitude
;; 返回复数的模。
;;
;; 语法
;; ----
;; (magnitude z)
;;
;; 参数
;; ----
;; z : number 复数或实数。
;;
;; 返回值
;; ----
;; real z 的绝对值（复数时为模）。
;;
;; 描述
;; ----
;; `magnitude` 对实数等价于 `abs`，对复数返回欧几里得模长。
;;
;; 数学定义
;; ------
;; 如果 z = x + yi，则 magnitude(z) = sqrt(x² + y²)。
;;
;; 示例
;; ----
;; (magnitude 3+4i) => 5
;; (magnitude -3) => 3
;; (magnitude 0) => 0
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是数值类型时抛出错误。
;; Test magnitude with complex numbers
(check (magnitude 3.0+4.0i) => 5.0)
;; Test magnitude with real numbers
(check (magnitude -3) => 3)
(check (magnitude -3.5) => 3.5)
(check (magnitude 0) => 0)
;; Error handling
(check-catch 'wrong-type-arg
  (magnitude "x")
) ;check-catch
(check-report)