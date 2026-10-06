(import (liii check) (liii match))

(check-set-mode! 'report-failed)

;; match-lambda
;; 创建一个单参数过程，并将该参数与给定的模式子句依次进行匹配。
;;
;; 语法
;; ----
;; (match-lambda (pattern body ...) ...)
;;
;; 等价于
;; ------
;; (lambda (expr) (match expr (pattern body ...) ...))
;;
;; 参数
;; ----
;; pattern : pattern
;; 模式子句。
;;
;; body : any
;; 匹配成功后依次执行的表达式序列。
;;
;; 返回值
;; -----
;; procedure
;; 一个接受单个参数的过程。

;; 基础测试 - 列表解构
(check
 ((match-lambda ((x y) (+ x y))) '(10 20))
 =>
 30
) ;check

;; 多分支测试
(check
 ((match-lambda ((x) (* x 2)) ((x y) (+ x y)) ((x y z) (* x y z))) '(3 4))
 =>
 7
) ;check

(check
 ((match-lambda ((x) (* x 2)) ((x y) (+ x y)) ((x y z) (* x y z))) '(5))
 =>
 10
) ;check

;; 谓词匹配
(check
 ((match-lambda ((? number? n) (+ n 1)) ((? string? s) (string-append s "!")))
  "hello"
 ) ;
 =>
 "hello!"
) ;check

;; 匹配失败抛出 match-error
(check-catch 'match-error ((match-lambda (('a) 'ok)) '(b)))

(check-report)
