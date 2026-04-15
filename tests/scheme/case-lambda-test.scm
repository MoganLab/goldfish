;; (scheme case-lambda) 模块文档
;;
;; `case-lambda` 提供基于参数数量的函数重载机制，
;; 允许一个函数根据不同数量的参数执行不同的逻辑。
;;
;; ==== 用途 ====
;;
;; case-lambda 用于根据参数数量匹配执行不同的代码逻辑，
;; 常用于提供参数默认值或实现参数数量相关的多态行为。
;;
;; ==== 语法 ====
;;
;;   (case-lambda
;;     (formals1 body1 ...)
;;     (formals2 body2 ...)
;;     ...)
;;
;;   formals: 参数列表，如 (x y) 或 (x y . rest)
;;   body:    对应参数数量的执行体
;;
;; ==== 匹配规则 ====
;;
;; - 根据实际传入的参数数量，选择对应子句执行
;; - 支持固定参数列表 (a b c) 和可变参数 (a b . rest)
;; - 如果没有匹配的子句，运行时抛出错误
;;
;; ==== 与 define* 的比较 ====
;;
;; define* 也提供参数默认值功能，且支持关键字参数 (gf doc liii/base "define*")。
;; 但 define* 的可选参数必须位于必需参数之后，灵活性不如 case-lambda。
;; 例如 my-range 中 (start end step) 且 start 有默认值，define* 无法方便实现。

(import (liii check)
        (scheme case-lambda)
) ;import

(check-set-mode! 'report-failed)

;; ==== 测试：基本用法 ====
;; 根据参数数量执行不同逻辑

(define my-func
  (case-lambda
    (() "zero args")
    ((x) (+ x x))
    ((x y) (+ x y))
    ((x y . rest) (apply + x y rest))
  ) ;case-lambda
) ;define

(check (my-func) => "zero args")
(check (my-func 2) => 4)
(check (my-func 3 4) => 7)
(check (my-func 1 2 3 4) => 10)

;; ==== 测试：参数默认值 ====
;; range 函数支持 1/2/3 个参数，未提供的参数使用默认值

(define my-range
  (case-lambda
    ((end) (my-range 0 end 1))
    ((start end) (my-range start end 1))
    ((start end step)
     (if (>= start end)
         '()
         (cons start (my-range (+ start step) end step))))
  ) ;case-lambda
) ;define

(check (my-range 5) => '(0 1 2 3 4))
(check (my-range 2 5) => '(2 3 4))
(check (my-range 0 10 2) => '(0 2 4 6 8))

(check-report)

