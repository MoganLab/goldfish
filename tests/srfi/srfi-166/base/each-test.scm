;; each
;; 把若干对象串成顺序输出的格式化组合子。
;;
;; 语法
;; ----
;; (each . args)
;;
;; 参数
;; ----
;; args : any
;; 零个或多个待输出对象：字符串、字符、数字、列表等，
;; 也可以是其他格式化组合子。
;;
;; 返回值
;; ----
;; format?
;; 按从左到右顺序输出全部参数的格式化组合子。
;;
;; 说明
;; ----
;; 每个参数按 displayed 语义输出，因此组合子会被求值、
;; 其余对象按 display/write 输出；each-in-list 接受列表。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (each "a" 1 #\b (list 2))) => "a1b(2)")
(check (show #f (each)) => "")
(check (show #f (each (each "x") "y")) => "xy")
(check (show #f (each "a" nothing "b")) => "ab")

(check-report)
