;; parameterize
;; 在动态作用域内临时替换参数对象的值。
;;
;; 语法
;; ----
;; (parameterize ((param value) ...) body ...)
;;
;; 参数
;; ----
;; param : procedure
;; 由 make-parameter 创建的参数对象。
;;
;; value : any
;; 参数在动态作用域内的新值。
;;
;; body : expression ...
;; 替换生效期间执行的表达式序列。
;;
;; 返回值
;; ----
;; any
;; 返回最后一个 body 表达式的求值结果。
;;
;; 说明
;; ----
;; parameterize 基于 dynamic-wind 实现：
;; 进入时替换参数值，退出时（无论正常返回还是异常展开）恢复原值。
;;
;; 嵌套使用 parameterize 时，内层退出后恢复的是外层的值。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-39))


(check-set-mode! 'report-failed)


(define mp (make-parameter "initial value"))

;; 作用域内取到新值
(check (parameterize ((mp "new value")) (mp)) => "new value")

;; 退出作用域后恢复原值
(check (mp) => "initial value")

;; 嵌套作用域
(check
  (parameterize ((mp "outer"))
    (list (mp)
          (parameterize ((mp "inner")) (mp))
          (mp)
    )
  ) ;parameterize
  =>
  '("outer" "inner" "outer")
) ;check

;; 作用域外的值不受影响
(check (mp) => "initial value")

;; 多个参数同时替换
(define p1 (make-parameter 1))
(define p2 (make-parameter 2))
(check (parameterize ((p1 10) (p2 20)) (list (p1) (p2))) => '(10 20))
(check (list (p1) (p2)) => '(1 2))


(check-report)
