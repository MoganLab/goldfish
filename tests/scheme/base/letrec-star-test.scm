(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; letrec*
;; 顺序创建递归绑定的局部变量，允许后续 init 使用前面绑定的值。
;;
;; 语法
;; ----
;; (letrec* ((var init) ...) body ...)
;;
;; 参数
;; ----
;; var : symbol
;; 变量名。
;;
;; init : any
;; 初始值表达式，可以使用之前定义的 var。
;;
;; body ... : any
;; 表达式体。
;;
;; 返回值
;; -----
;; any
;; 返回最后一个 body 表达式的结果。
;;
;; 说明
;; ----
;; letrec* 与 letrec 类似，但 init 表达式是按顺序求值的，
;; 后续的 init 可以使用前面已经绑定的 var 的值。

;; 基础测试 - 后续绑定使用前面绑定的值
(check
  (letrec* ((a 1) (b (+ a 1))) (list a b))
  => (list 1 2))

;; 与 letrec 的区别：letrec 不允许立即使用其他绑定值
;; letrec* 允许，因为它按顺序求值

(check-report)
