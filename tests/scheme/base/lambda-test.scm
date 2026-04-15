(import (liii check))
(import (liii list))
(import (scheme base))
(check-set-mode! 'report-failed)
;; lambda
;; 测试 lambda 表达式、闭包和简单高阶调用。
;;
;; 语法
;; ----
;; ((lambda (...) ...) ...)
;;
;; 参数
;; ----
;; 取决于具体 lambda 形参定义。
;;
;; 返回值
;; ----
;; any?
;; 返回 lambda 体的计算结果。
;;
;; 注意
;; ----
;; 本文件覆盖匿名函数调用、嵌套 lambda、闭包捕获和未绑定变量错误。
;;
;; 示例
;; ----
;; ((lambda (x) (* x x)) 5) => 25
;;
;; 错误处理
;; ----
;; unbound-variable 当访问未绑定变量时
(check ((lambda (x) (* x x)) 5) => 25)
(check ((lambda (x) (* x x)) 0) => 0)
(check ((lambda (x) (* x x)) -3) => 9)
(check ((lambda (x y) (+ x y)) 3 5)
  =>
  8
) ;check
(check ((lambda (x y) (* x y)) 4 6)
  =>
  24
) ;check
(check ((lambda () 42)) => 42)
(check ((lambda (x) ((lambda (y) (+ x y)) 5))
        3
       ) ;
  =>
  8
) ;check
(define (apply-function f x)
  (f x)
) ;define
(check (apply-function (lambda (x) (* x x)) 5)
  =>
  25
) ;check
(check (apply-function (lambda (x) (+ x 1)) 10)
  =>
  11
) ;check
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst)))
        ) ;
        (else (filter pred (cdr lst)))
  ) ;cond
) ;define
(let ((create-counter (lambda ()
                        (let ((count 0))
                          (lambda ()
                            (set! count (+ count 1))
                            count
                          ) ;lambda
                        ) ;let
                      ) ;lambda
      ) ;create-counter
     ) ;
  (let ((counter1 (create-counter))
        (counter2 (create-counter))
       ) ;
    (counter1)
    (counter1)
    (counter2)
    (check (counter1) => 3)
    (check (counter2) => 2)
  ) ;let
) ;let
(check-catch 'unbound-variable
 ((lambda (x) y) 5)
) ;check-catch
(check-report)
