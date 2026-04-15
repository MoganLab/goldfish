(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; when
;; 当条件为真时执行表达式序列。
;;
;; 语法
;; ----
;; (when test-expr body ...)
;;
;; 参数
;; ----
;; test-expr : any
;; 条件表达式。除了 #f 之外的所有值都被视为真值。
;;
;; body ... : any
;; 当 test-expr 为真时要执行的一个或多个表达式。
;;
;; 返回值
;; -----
;; any
;; 如果条件为真，返回最后一个 body 表达式的结果
;; 如果条件为假，返回未指定值（#<unspecified>）
;; 基础测试 - 条件为真
(check (when #t 1) => 1)
;; 基础测试 - 条件为假，返回未指定值
(check (when #f 1) => #<unspecified>)
;; 表达式测试 - 条件为真
(check (when (> 3 1) 1) => 1)
;; 表达式测试 - 条件为假
(check (when (> 1 3)
         1
       ) ;when
  =>
  #<unspecified>
) ;check
;; 多表达式测试
(check (let ((result '()))
         (when #t
           (set! result (cons 'a result))
           (set! result (cons 'b result))
           result
         ) ;when
       ) ;let
  =>
  '(b a)
) ;check
(check-report)