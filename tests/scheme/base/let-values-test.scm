(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; let-values
;; 绑定多个值到多个变量，用于接收多返回值。
;;
;; 语法
;; ----
;; (let-values (((var ...) init) ...) body ...)
;;
;; 参数
;; ----
;; (var ...) : 变量列表
;; 要绑定的变量名列表。
;;
;; init : expression returning multiple values
;; 返回多个值的表达式，通常使用 (values ...)。
;;
;; body ... : any
;; 表达式体。
;;
;; 返回值
;; -----
;; any
;; 返回最后一个 body 表达式的结果。
;; 单值绑定
(check (let-values (((ret) (+ 1 2)))
         (+ ret 4)
       ) ;let-values
  =>
  7
) ;check
;; 多值绑定
(check (let-values (((a b) (values 3 4)))
         (+ a b)
       ) ;let-values
  =>
  7
) ;check
;; 多组绑定
(check (let-values (((x y) (values 1 2))
                    ((z w) (values 3 4))
                   ) ;
         (+ x y z w)
       ) ;let-values
  =>
  10
) ;check
;; 嵌套使用
(check (let-values (((a b) (values 1 2)))
         (let-values (((c d) (values (+ a b) (* a b))))
           (+ c d)
         ) ;let-values
       ) ;let-values
  =>
  5
) ;check
(check-report)
