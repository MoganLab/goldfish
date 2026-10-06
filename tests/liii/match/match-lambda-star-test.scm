(import (liii check) (liii match))

(check-set-mode! 'report-failed)

;; match-lambda*
;; 创建一个接受任意数量参数的过程，并将整个参数列表与给定的模式子句依次进行匹配。
;;
;; 语法
;; ----
;; (match-lambda* (pattern body ...) ...)
;;
;; 等价于
;; ------
;; (lambda expr (match expr (pattern body ...) ...))
;;
;; 参数
;; ----
;; pattern : pattern
;; 模式子句，针对传递给该过程的实参列表整体进行匹配。
;;
;; body : any
;; 匹配成功后依次执行的表达式序列。
;;
;; 返回值
;; -----
;; procedure
;; 一个接受任意数量实参的过程。

;; 基础测试 - 双参数解构
(check
 ((match-lambda* ((x y) (+ x y))) 10 20)
 =>
 30
) ;check

;; 可变参数匹配 - 省略号
(check
 ((match-lambda* ((x ... y) (list x y))) 1 2 3 4)
 =>
 '((1 2 3) 4)
) ;check

;; 空参数与单参数重载
(check
 ((match-lambda* (() 'zero) ((x) (list 'one x)) ((x y) (list 'two x y))))
 =>
 'zero
) ;check

(check
 ((match-lambda* (() 'zero) ((x) (list 'one x)) ((x y) (list 'two x y))) 42)
 =>
 '(one 42)
) ;check

;; 匹配失败抛出 match-error
(check-catch 'match-error ((match-lambda* ((x y) (+ x y))) 1))

(check-report)
