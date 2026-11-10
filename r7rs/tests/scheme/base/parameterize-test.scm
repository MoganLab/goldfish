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
;; 参数在动态作用域内的新值（经 converter 转换后生效）。
;;
;; 返回值
;; ----
;; any
;; body 最后一个表达式的求值结果；body 处于尾位置。
;;
;; 描述
;; ----
;; 1. parameterize 基于 dynamic-wind 实现：进入时替换参数值，
;;    退出时（正常返回或异常展开）恢复原值。
;; 2. 恢复时直接写回旧值，不经 converter 二次转换。
;; 3. 嵌套使用时，内层退出后恢复的是外层的值。
;; 4. 空绑定列表等价于 (let () body ...)。
;;
;; 错误处理
;; --------
;; 无。
;;
;; (define p (make-parameter 1))
;; (parameterize ((p 2)) (p)) => 2

(import (liii check) (scheme base))
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

;; 空绑定列表
(check (parameterize () 42) => 42)

;; 异常展开时恢复原值
(define p3 (make-parameter 3))
(check
  (guard (e (#t 'caught))
    (parameterize ((p3 99))
      (raise 'boom)
    ) ;parameterize
  ) ;guard
  =>
  'caught
) ;check
(check (p3) => 3)

;; converter 在 parameterize 写入时生效，恢复时不二次转换
(define p4 (make-parameter 1 (lambda (x) (* x 10))))
(check (p4) => 10)
(check (parameterize ((p4 5)) (p4)) => 50)
(check (p4) => 10)

;; body 处于尾位置：深层递归不累积栈
(define (count-down n)
  (parameterize ((p2 n))
    (if (= n 0) 'done (count-down (- n 1)))
  ) ;parameterize
) ;define
(check (count-down 100000) => 'done)

(check-report)
