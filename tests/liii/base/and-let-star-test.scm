(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

;; and-let*
;; 顺序绑定变量或执行断言，如果任一子句结果为假则立即短路返回假，否则执行主体。
;;
;; 语法
;; ----
;; (and-let* (claw ...) body ...)
;; (and-let* (claw ...))
;;
;; 其中 claw 的形式可以为：
;; - (var expr) : 求值 expr，若非 #f 则绑定到局部变量 var
;; - (expr)     : 条件断言，求值 expr，若为 #f 则短路终止
;; - var        : 变量断言，读取已绑定的 var，若为 #f 则短路终止
;;
;; 参数
;; ----
;; claw : list 或 symbol
;; 绑定或断言子句。
;;
;; body : any
;; 当所有子句都成功时执行的表达式序列；若省略 body，则返回最后一个子句的结果。
;;
;; 返回值
;; -----
;; any
;; - 如果任一子句求值为假，立即短路返回 #f
;; - 如果所有子句都为真且提供了 body，返回最后一个 body 表达式的结果
;; - 如果省略 body，返回最后一个子句的值（空绑定返回 #t）
;;
;; 说明
;; ----
;; and-let* 遵循 SRFI-2 规范，结合了 and 和 let* 的功能。
;; 适合用于多步骤带依赖的条件前置检查与临时变量绑定。

;; 1. 基础用法：顺序变量绑定与主体执行
(check (and-let* ((hi 3) (ho #t)) (+ hi 1)) => 4)
(check (and-let* ((hi 3) (ho #f)) (+ hi 1)) => #f)
(check (and-let* ((x 5)) (* x 2)) => 10)
(check
  (and-let* ((a 1) (b (+ a 2)) (c (* b 2))) (+ a b c))
  =>
  10
) ;check

;; 2. 条件断言子句 (expr)
(check
  (and-let* (((> 2 1))) 'ok)
  =>
  'ok
) ;check
(check
  (and-let* (((< 2 1))) 'ok)
  =>
  #f
) ;check
(check
  (and-let* ((x 10) ((> x 5))) (* x 2))
  =>
  20
) ;check
(check
  (and-let* ((x 3) ((> x 5))) (* x 2))
  =>
  #f
) ;check

;; 3. 已绑定变量断言 var
(check (let ((flag #t)) (and-let* (flag) 'yes)) => 'yes)
(check (let ((flag #f)) (and-let* (flag) 'yes)) => #f)
(check
  (let ((flag #t))
    (and-let* ((x 1) flag) (+ x 10))
  ) ;let
  =>
  11
) ;check
(check
  (let ((flag #f))
    (and-let* ((x 1) flag) (+ x 10))
  ) ;let
  =>
  #f
) ;check

;; 4. 短路求值行为（任一子句失败后，后续子句及主体不予执行）
(check
  (let ((evaluated #f))
    (and-let*
     ((x 1) (y #f) (z (begin (set! evaluated #t) 3)))
     'body
    ) ;and-let*
    evaluated
  ) ;let
  =>
  #f
) ;check

(check
  (let ((count 0))
    (and-let*
     (((begin (set! count (+ count 1)) #f)) (y (begin (set! count (+ count 1)) #t)))
     'body
    ) ;and-let*
    count
  ) ;let
  =>
  1
) ;check

;; 5. 边界情况：无 body（返回最后一个子句的求值结果）
;; (1) 单子句无 body
(check (and-let* ((x 1))) => 1)
(check (and-let* ((x #f))) => #f)
(check
  (and-let* (((> 2 1))))
  =>
  #t
) ;check
(check
  (and-let* (((> 1 2))))
  =>
  #f
) ;check
(check (let ((x 42)) (and-let* (x))) => 42)
(check (let ((x #f)) (and-let* (x))) => #f)

;; (2) 多子句无 body
(check (and-let* ((x 1) (y 2))) => 2)
(check (and-let* ((x 1) (y #f))) => #f)
(check (and-let* ((x #f) (y 2))) => #f)
(check
  (and-let* ((x 1) ((> 2 1))))
  =>
  #t
) ;check
(check
  (and-let* ((x 1) ((< 2 1))))
  =>
  #f
) ;check
(check
  (let ((z 100))
    (and-let* ((x 1) z))
  ) ;let
  =>
  100
) ;check
(check
  (let ((z #f))
    (and-let* ((x 1) z))
  ) ;let
  =>
  #f
) ;check

;; 6. 边界情况：空子句列表
(check (and-let* ()) => #t)
(check (and-let* () 42) => 42)
(check (and-let* () 1 2) => 2)

(check-report)
