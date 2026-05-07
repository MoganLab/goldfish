(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;; cdr
;; cdr 是 Scheme 内置函数，用于获取序对的第二个元素。该函数与 car 配合使用，是 R7RS 标准的基本列表操作函数之一。
;;
;; 语法
;; ----
;; (cdr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 可以是序对（即非空列表或显式点对），不能是空列表或其他对象。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回序对的第二个元素（cdr 部分）。
;;
;; 说明
;; ----
;; 1. 当应用于列表时，返回列表除第一个元素外的子列表
;; 2. 适用于所有序对数据：不论是点对 (a . b) 还是非空列表 (a b c ...)
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是序对时抛出错误。

(check (cdr '(a b c . d)) => '(b c . d))
(check (cdr '(a b c)) => '(b c))
(check (cdr '(1 . 2)) => 2)
(check (cdr '((a b) . c)) => 'c)
(check (cdr (cons 1 2)) => 2)
(check (cdr (cons 'a 'b)) => 'b)
(check-catch 'wrong-type-arg (cdr '()))
(check-catch 'wrong-type-arg (cdr 123))
(check-catch 'wrong-type-arg (cdr "hello"))
(check-catch 'wrong-type-arg (cdr #t))
(check-catch 'wrong-number-of-args (cdr))
(check-catch 'wrong-number-of-args (cdr '(1 2) '(3 4)))
;; cdr边界条件测试补充
(check (cdr '(a)) => '())
(check (cdr '(1)) => '())
(check (cdr '(#t)) => '())
(check (cdr '("hello")) => '())
(check (cdr '(() b c)) => '(b c))
(check (cdr '((a b))) => '())
(check (cdr '((((a))))) => '())
;; 各种数据类型cdr边界测试
(check (cdr '(123 "text" symbol)) => '("text" symbol))
(check (cdr '(#\newline
              #	ab
              #\space)
       ) ;cdr
  =>
  '(#	ab #\space)
) ;check

(check (cdr '((a b) c d)) => '(c d))
(check (cdr '(#(1 2) #(3 4))) => '(#(3 4)))
(check (cdr '(+ - * /)) => '(- * /))
(check (cdr '((#_quote (a b)) (#_quote (c d)))) => '((#_quote (c d))))

;; 极端边界条件测试
(check (cdr '((lambda (x) x) (lambda (y) y))) => '((lambda (y) y)))
(check (cdr '((begin 1 2 3) (begin 4 5))) => '((begin 4 5)))
(check (cdr '(a b.c d)) => '(b.c d))
(check (cdr '("中文" "测试")) => '("测试"))
(check-report)
