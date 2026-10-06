(import (scheme base)
        (srfi 78))

(check-set-mode! 'report-failed)

;; letrec-syntax
;; 在局部词法作用域中引入支持递归引用的语法关键字绑定。
;;
;; 语法
;; ----
;; (letrec-syntax ((<keyword> <transformer-spec>) ...) <body> ...)
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

;; 测试1：递归宏绑定
(check
  (letrec-syntax ((my-or (syntax-rules ()
                           ((my-or) #f)
                           ((my-or e) e)
                           ((my-or e1 e2 ...)
                            (if e1 e1 (my-or e2 ...))))))
    (my-or #f #f 42))
  => 42)

;; 测试2：递归列表重构
(check
  (letrec-syntax ((my-reverse (syntax-rules ()
                                ((my-reverse () acc) acc)
                                ((my-reverse (head . tail) acc)
                                 (my-reverse tail (cons head acc))))))
    (my-reverse (1 2 3) '()))
  => '(3 2 1))

(check-report)
