(import (scheme base)
        (srfi 78))

(check-set-mode! 'report-failed)

;; let-syntax
;; 在局部词法作用域中引入语法关键字绑定。
;;
;; 语法
;; ----
;; (let-syntax ((<keyword> <transformer-spec>) ...) <body> ...)
;;
;; 参数
;; ----
;; <keyword> : identifier
;; 局部语法关键字。
;;
;; <transformer-spec> : transformer
;; 宏转换器规格说明，如 (syntax-rules ...)。
;;
;; <body> ... : any
;; 在新语法环境中展开的一个或多个表达式序列。
;;
;; 返回值
;; -----
;; any
;; 返回最后一个 body 表达式的求值结果。

;; 测试1：基础局部宏绑定
(check
  (let-syntax ((add1 (syntax-rules ()
                       ((add1 x) (+ x 1)))))
    (add1 5))
  => 6)

;; 测试2：多个局部宏互不相干展开
(check
  (let-syntax ((one (syntax-rules () ((one) 1)))
               (two (syntax-rules () ((two) 2))))
    (+ (one) (two)))
  => 3)

(check-report)
