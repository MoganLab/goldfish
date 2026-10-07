(import (scheme base)
        (srfi 78))

(check-set-mode! 'report-failed)

;; let*-values
;; 顺序绑定多个值到多个变量，后一个绑定可使用前一个绑定的变量。
;;
;; 语法
;; ----
;; (let*-values (((var ...) init) ...) body ...)
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

;; 空绑定测试
(check (let*-values () 42) => 42)

;; 单绑定测试
(check (let*-values (((ret) (+ 1 2))) (+ ret 4)) => 7)

;; 多值多绑定测试
(check (let*-values (((a b) (values 1 2))
                     ((c d) (values (+ a b) (* a b))))
         (+ c d))
  => 5)

;; 顺序依赖测试：后一个绑定依赖前一个绑定
(check (let*-values (((x y) (values 10 20))
                     ((z) (values (+ x y)))
                     ((w) (values (* z 2))))
         (+ x y z w))
  => 120)

;; SRFI-11 官方用例：顺序求值与顺序绑定语义验证
(check (let ((a 'a) (b 'b) (x 'x) (y 'y))
         (let*-values (((a b) (values x y))
                       ((x y) (values a b)))
           (list a b x y)))
  => '(x y x y))

;; 点对可变参数多值顺序绑定
(check (let*-values (((a b . c) (values 1 2 3 4))
                     ((d) (+ a b)))
         (list d c))
  => '(3 (3 4)))

(check-report)

