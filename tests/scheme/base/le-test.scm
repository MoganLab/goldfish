(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; <=
;; 比较数字是否按非降序排列。
;;
;; 语法
;; ----
;; (<= num1 num2 ...)
;;
;; 参数
;; ----
;; num1, num2, ... : number?
;; 待比较的数字。
;;
;; 返回值
;; ------
;; boolean?
;; 如果每个参数都小于或等于后面的参数，返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 至少需提供两个参数
;; 2. 允许相等关系
;; 3. 不支持复数比较
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 参数数量不足两个时抛出。
;; wrong-type-arg
;; 任一参数不是实数时抛出。
(check (<= 1 2) => #t)
(check (<= 2 1) => #f)
(check (<= 1 1) => #t)
(check (<= 1 2 3) => #t)
(check (<= 1 2 2) => #t)
(check (<= 1 2 1) => #f)
(check (<= -5 -3) => #t)
(check (<= -3 -5) => #f)
(check (<= 0 1) => #t)
(check (<= 1.5 2.5) => #t)
(check (<= 2.5 1.5) => #f)
(check (<= 1/2 3/4) => #t)
(check (<= 3/4 1/2) => #f)
(check (<= 1 1.1) => #t)
(check (<= 1.1 1) => #f)
(check (<= 0.0 0.0) => #t)
(check-catch 'wrong-number-of-args (<=))
(check-catch 'wrong-number-of-args (<= 1))
(check-catch 'wrong-type-arg (<= 1 'a))
(check-catch 'wrong-type-arg (<= "hello" 2))
(check-report)
