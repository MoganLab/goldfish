(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; let*
;; 顺序绑定局部变量，后续绑定可以使用前面绑定的变量。
;;
;; 语法
;; ----
;; (let* ((var init) ...) body ...)
;;
;; 参数
;; ----
;; var : symbol
;; 变量名。
;;
;; init : any
;; 初始值表达式，可以使用之前定义的变量。
;;
;; body ... : any
;; 表达式体。
;;
;; 返回值
;; -----
;; any
;; 返回最后一个 body 表达式的结果。
;; 基础测试 - 顺序绑定
(check (let* ((x 10) (y (+ x 5))) y) => 15)
;; 多层嵌套绑定
(check (let* ((a 1) (b (+ a 1)) (c (* b 2)))
         (* a b c)
       ) ;let*
  =>
  8
) ;check
;; 变量更新 - 同名变量遮蔽
(check (let* ((x 1) (x (+ x 1)) (x (* x 2))) x) => 4)
;; 空绑定
(check (let* () "result") => "result")
;; 作用域测试
(check (let* ((x 10)) (let* ((y (+ x 5))) (+ x y))) => 25)
;; 嵌套 let*
(check (let* ((a 1) (b 2))
         (let* ((c (+ a b)) (d (* a b c)))
           (+ a b c d)
         ) ;let*
       ) ;let*
  =>
  12
) ;check
;; 闭包测试
(check (let ((x 1))
         (let* ((y (+ x 1)) (z (lambda () (+ x y))))
           (z)
         ) ;let*
       ) ;let
  =>
  3
) ;check
;; 副作用测试
(check (let ((counter 0))
         (let* ((a (begin (set! counter (+ counter 1)) 10))
                (b (begin (set! counter (+ counter 1)) 20))
               ) ;
           counter
         ) ;let*
       ) ;let
  =>
  2
) ;check
;; 类型混用
(check (let* ((s "Hello")
              (len (string-length s))
              (lst (cons len (cons s '())))
             ) ;
         lst
       ) ;let*
  =>
  '(5 "Hello")
) ;check
;; 错误用法 - 引用未定义变量
(check-catch 'unbound-variable (let* ((x y) (y 10)) x))
;; 复杂表达式
(check (let* ((x (if #t 10 20)) (y (let* ((a x) (b (+ a 5))) (+ a b))))
         y
       ) ;let*
  =>
  25
) ;check
(check-report)