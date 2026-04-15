(import (liii check) (scheme complex))
(check-set-mode! 'report-failed)
;; make-polar
;; 按极坐标构造复数。
;;
;; 语法
;; ----
;; (make-polar magnitude angle)
;;
;; 参数
;; ----
;; magnitude : real 模长。
;; angle : real 辐角（弧度）。
;;
;; 返回值
;; ----
;; number 按极坐标换算得到的复数。
;;
;; 描述
;; ----
;; `make-polar` 根据模长和辐角构造复数，等价于
;; `(complex (* magnitude (cos angle)) (* magnitude (sin angle)))`。
;;
;; 数学定义
;; ------
;; z = magnitude * (cos(angle) + i*sin(angle))
;;
;; 示例
;; ----
;; (make-polar 2 0) 的实部为 2，虚部为 0
;; (make-polar 1 π/2) 的实部接近 0，虚部接近 1
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是实数类型时抛出错误。
;; wrong-number-of-args 当参数个数错误时抛出错误。
;; Test make-polar
(check (real-part (make-polar 2 0))
  =>
  2.0
) ;check
(check (imag-part (make-polar 2 0))
  =>
  0.0
) ;check
(check (> (real-part (make-polar 1 1.5707963267948966)
          ) ;real-part
         -0.001
       ) ;>
  =>
  #t
) ;check
(check (< (real-part (make-polar 1 1.5707963267948966)
          ) ;real-part
         0.001
       ) ;<
  =>
  #t
) ;check
(check (> (imag-part (make-polar 1 1.5707963267948966)
          ) ;imag-part
         0.999
       ) ;>
  =>
  #t
) ;check
;; Error handling
(check-catch 'wrong-type-arg
  (make-polar "x" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-polar 1 "x")
) ;check-catch
(check-catch 'wrong-number-of-args
  (make-polar 1)
) ;check-catch
(check-report)
