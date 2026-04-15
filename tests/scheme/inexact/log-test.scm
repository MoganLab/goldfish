(import (liii check) (scheme inexact))
(check-set-mode! 'report-failed)
;; log
;; 计算对数函数。单个参数时计算自然对数(log base e)，两个参数时计算以第二个参数为底的对数。
;;
;; 语法
;; ----
;; (log z [base])
;;
;; 参数
;; ----
;; z : number?
;; 必须为数，计算对数值
;;
;; base : number? 可选
;; 必须为数，表示对数底
;;
;; 返回值
;; ------
;; number?
;; 对应的对数值
;;
;; 注意
;; ----
;; 1. 单个参数：计算自然对数ln(z) = log_e(z)
;; 2. 两个参数：计算log_base(z) = log(z)/log(base)
;; 3. 支持各种数值类型
;; 4. 注意参数必须为正数且不等于1
;;
;; 错误处理
;; ------
;; out-of-range
;; 当z <= 0或base <= 0时抛出错误。
;; wrong-type-arg
;; 当参数类型错误时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1或2个时抛出错误。
;; log 基本自然对数测试
(check (log 1) => 0.0)
(check (log (exp 1)) => 1.0)
(check (log 2) => 0.6931471805599453)
;; log 双参数对数测试
(check (log 100 10) => 2)
(check (log 8 2) => 3)
(check (log 16 2) => 4)
;; log 通用对数测试
(check (log 10 10) => 1)
(check (log 100 10) => 2)
(check (log 1 10) => 0)
;; log 有理数测试
(check (log 2 4) => 1/2)
(check (log 1/2 2) => -1.0)
(check (log 9 3) => 2)
;; log 浮点数对数测试
(check (log 2.718281828459045) => 1.0)
(check (log 0.1 10)
  =>
  -0.9999999999999998
) ;check
;; 相互验证测试
(check (log (exp 3)) => 3.0)
(check (exp (log 5))
  =>
  4.999999999999999
) ;check
;; 错误处理测试
(check (log 0)
  =>
  -inf.0+3.141592653589793i
) ;check
(check (log -1)
  =>
  0.0+3.141592653589793i
) ;check
(check (log 3 1) => +inf.0)
(check-catch 'out-of-range (log 10 0))
(check-catch 'wrong-type-arg (log "a"))
(check-catch 'wrong-number-of-args
  (log)
) ;check-catch
(check-catch 'wrong-number-of-args
  (log 12 4 5)
) ;check-catch
(check-report)