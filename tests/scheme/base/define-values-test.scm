(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; define-values
;; 定义多个变量，绑定到一个返回多个值的表达式的结果。
;;
;; 语法
;; ----
;; (define-values (var ...) expr)
;;
;; 参数
;; ----
;; var ... : symbol
;; 要定义的变量名列表。
;;
;; expr : expression returning multiple values
;; 返回多个值的表达式，通常使用 (values ...)。
;;
;; 说明
;; ----
;; define-values 用于解构多值表达式，将多个返回值绑定到多个变量。
;; 它是 let-values 的顶层定义版本。
;; 基础测试 - 单值
(let ()
  (define-values (value1)
    (+ 1 2)
  ) ;define-values
  (check value1 => 3)
) ;let
;; 多值绑定
(let ()
  (define-values (a b)
    (values 1 2)
  ) ;define-values
  (check a => 1)
  (check b => 2)
) ;let
;; 使用多值计算结果
(let ()
  (define-values (x y)
    (values 3 4)
  ) ;define-values
  (check (+ x y) => 7)
) ;let
;; 多个 define-values
(let ()
  (define-values (a b)
    (values 1 2)
  ) ;define-values
  (define-values (c d)
    (values 3 4)
  ) ;define-values
  (check (+ a b c d) => 10)
) ;let
;; 嵌套在 let 中
(check (let ((result (let ()
                       (define-values (x y z)
                         (values 1 2 3)
                       ) ;define-values
                       (+ x y z)
                     ) ;let
             ) ;result
            ) ;
         result
       ) ;let
  =>
  6
) ;check
(check-report)
