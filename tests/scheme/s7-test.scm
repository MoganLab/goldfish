(import (liii check))
(check-set-mode! 'report-failed)

;;
;; define-constant
;; 定义不可变常量，一旦定义后不能修改或重新绑定。
;;
;; 语法
;; ----
;; (define-constant name value)
;;
;; 参数
;; ----
;; name : symbol
;; 要定义的常量名称，必须是符号。
;;
;; value : any
;; 常量的值，可以是任意类型。
;;
;; 返回值
;; ----
;; name
;; 返回定义的常量名称。
;;
;; 描述
;; ----
;; define-constant 用于定义不可变常量。一旦常量被定义，在相同的作用域内不能重新绑定
;; 或修改该符号的值。这提供了比普通 define 更强的保护，确保某些值在整个程序执行
;; 过程中保持不变。
;;
;; 特点
;; ----
;; - 常量一旦定义后不可修改
;; - 在相同作用域内不能重新绑定常量名称
;; - 支持定义函数常量
;; - 遵循词法作用域规则
;;
;; 注意事项
;; ----
;; - 尝试修改常量会抛出错误
;; - 在相同作用域内重新定义常量会抛出错误
;; - 常量定义是词法作用域的，不会影响外部作用域的同名变量
;; - 可以使用 constant? 函数检查符号是否为常量

(check (let ()
         (define-constant PI 3.14159)
         PI
       ) ;let
  =>
  3.14159
) ;check
(check (let ()
         (define-constant GREETING "Hello")
         GREETING
       ) ;let
  =>
  "Hello"
) ;check
(check (let ()
         (define-constant ANSWER 42)
         ANSWER
       ) ;let
  =>
  42
) ;check
(check (let ()
         (define-constant (square x) (* x x))
         (square 5)
       ) ;let
  =>
  25
) ;check
(check (let ()
         (define-constant (add x y) (+ x y))
         (add 3 4)
       ) ;let
  =>
  7
) ;check
(check-catch 'immutable-error
  (let ()
    (define-constant X 1)
    (set! X 2)
  ) ;let
) ;check-catch
(check-catch 'immutable-error
  (let ()
    (define-constant Y "test")
    (set! Y "new")
  ) ;let
) ;check-catch
(check-catch 'immutable-error
  (let ()
    (define-constant (func x) x)
    (set! func (lambda (x) (+ x 1)))
  ) ;let
) ;check-catch
(check (let ()
         (define-constant TEST-CONST 123)
         (constant? 'TEST-CONST)
       ) ;let
  =>
  #t
) ;check
(check (let ()
         (define TEST-VAR 456)
         (constant? 'TEST-VAR)
       ) ;let
  =>
  #f
) ;check
(check-catch 'syntax-error
  (define-constant)
) ;check-catch
(check-catch 'syntax-error
  (define-constant NAME)
) ;check-catch
(check (let ()
         (define-constant (factorial n)
           (if (<= n 1)
             1
             (* n (factorial (- n 1)))
           ) ;if
         ) ;define-constant
         (factorial 5)
       ) ;let
  =>
  120
) ;check
(check (let ()
         (define-constant (make-adder x)
           (lambda (y) (+ x y))
         ) ;define-constant
         ((make-adder 10) 5)
       ) ;let
  =>
  15
) ;check
(check (let ((x 1))
         (define-constant y 2)
         (+ x y)
       ) ;let
  =>
  3
) ;check
(check-report)