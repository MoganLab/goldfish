(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; lambda*
;; 创建带有默认参数和关键字参数的函数。
;;
;; 语法
;; ----
;; (lambda* args body ...)
;;
;; 参数
;; ----
;; args - 参数列表，可包含默认值，如 (a (b 2))
;; body - 函数体
;;
;; 返回值
;; ------
;; 一个过程对象。
;;
;; 说明
;; ----
;; lambda* 是 S7 内置函数，但不在 R7RS 标准中定义。
;; 支持通过关键字传入参数，如 (:b 5)。
;;
;; 示例
;; ----
;; ((lambda* (a (b 2)) (+ a b)) 1)       => 3
;; ((lambda* (a (b 2)) (+ a b)) 1 :b 5)  => 6


(check ((lambda* (a (b 2)) (+ a b)) 1) => 3)
(check ((lambda* (a (b 2)) (+ a b)) 1 5) => 6)
(check ((lambda* (a (b 2)) (+ a b)) 1 :b 5) => 6)
(check ((lambda* ((name "World")) (string-append "Hello, " name))) => "Hello, World")
(check (procedure? (lambda* (x) x)) => #t)


(check-report)
