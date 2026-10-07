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
;; - 如果省略 body，返回最后一个子句的值（无子句时返回 #t）
;;
;; 说明
;; ----
;; and-let* 遵循 SRFI-2 规范，是 and 与 let* 的结合体（亦称带守卫的 let*）。
;;
;; 为什么需要 and-let*？
;; 1. 普通 and 的局限：在 (and E1 E2 ...) 中，E1 计算出一个非 #f 的有价值结果
;;    （如查找到的节点、解析出的数据），and 仅检验其非 #f 后便将其丢弃；如果 E2
;;    后续还需要用到该值，就必须重新计算一遍，造成重复计算。
;; 2. 普通 let* 的局限：let* 虽可绑定变量供后续使用，但缺乏“守卫”短路机制。若
;;    某一中间步骤失败返回 #f，let* 无法自动中止，后续表达式会继续盲目执行。
;; 3. and-let* 的解决方式：兼具 let* 的局部变量绑定能力与 and 的条件短路能力。
;;    它在求值每个子句时校验结果是否为真，一旦遇到 #f 立即中止并返回 #f；若为
;;    真值则绑定并供后续子句与主体直接复用，从而避免重复计算。
;;
;; 典型场景：
;; - 关联列表安全查询：(and-let* ((x (assq key alist))) (cdr x))
;; - 多步骤依赖校验与数据提取（如逐层解构、数据校验与转换流水线）。

;; 1. 基础用法：顺序变量绑定与主体执行
(check (and-let* ((hi 3) (ho #t)) (+ hi 1)) => 4)
(check (and-let* ((hi 3) (ho #f)) (+ hi 1)) => #f)
(check (and-let* ((x 5)) (* x 2)) => 10)
(check
  (and-let* ((a 1) (b (+ a 2)) (c (* b 2))) (+ a b c))
  =>
  10
) ;check

;; 典型应用：关联表安全查询（命中则取值，未命中 assq 返回 #f 触发短路）
(check
  (let ((lookup (lambda (key alist) (and-let* ((x (assq key alist))) (cdr x)))))
    (lookup 'b '((a . 1) (b . 2) (c . 3)))
  ) ;let
  =>
  2
) ;check

(check
  (let ((lookup (lambda (key alist) (and-let* ((x (assq key alist))) (cdr x)))))
    (lookup 'd '((a . 1) (b . 2) (c . 3)))
  ) ;let
  =>
  #f
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
