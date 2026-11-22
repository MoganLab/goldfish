;; let*-values
;; 顺序绑定多个值到多个变量，后一个绑定可使用前一个绑定的变量。
;;
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
;; ----
;; any
;; 返回最后一个 body 表达式的结果。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 空绑定测试
(check (let*-values () 42) => 42)

;; 空绑定时 body 中的 define 不应泄漏到外层作用域
;; R7RS 规定 <body> 有独立作用域，与空 let 一致
(check
  (let ((x 1))
    (let*-values ()
      (define x 2)
      #f
    ) ;let*-values
    x
  ) ;let
  =>
  1
) ;check

;; 单绑定测试
(check
  (let*-values (((ret) (+ 1 2)))
    (+ ret 4)
  ) ;let*-values
  =>
  7
) ;check

;; 多值多绑定测试
(check
  (let*-values (((a b) (values 1 2)) ((c d) (values (+ a b) (* a b))))
    (+ c d)
  ) ;let*-values
  =>
  5
) ;check

;; 顺序依赖测试：后一个绑定依赖前一个绑定
(check
  (let*-values (((x y) (values 10 20)) ((z) (values (+ x y))) ((w) (values (* z 2))))
    (+ x y z w)
  ) ;let*-values
  =>
  120
) ;check

;; SRFI-11 官方用例：顺序求值与顺序绑定语义验证
(check
  (let ((a 'a) (b 'b) (x 'x) (y 'y))
    (let*-values (((a b) (values x y)) ((x y) (values a b)))
      (list a b x y)
    ) ;let*-values
  ) ;let
  =>
  '(x y x y)
) ;check

;; 点对可变参数多值顺序绑定
(check
  (let*-values (((a b . c) (values 1 2 3 4)) ((d) (+ a b)))
    (list d c)
  ) ;let*-values
  =>
  '(3 (3 4))
) ;check

(check-report)
