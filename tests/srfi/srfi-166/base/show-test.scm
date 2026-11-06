;; show
;; 组合子格式化的主运行函数，把格式化对象渲染到端口或字符串。
;;
;; 语法
;; ----
;; (show out . args)
;;
;; 参数
;; ----
;; out : output-port? | boolean?
;; `#f` 表示把结果作为字符串返回；`#t` 表示输出到
;; `(current-output-port)`；其他值必须是输出端口。
;;
;; args : any
;; 零个或多个格式化对象：字符串、字符、数字，
;; 或由本库组合子构造出的 computation。
;;
;; 返回值
;; ----
;; string? | unspecified
;; out 为 `#f` 时返回渲染出的字符串，否则返回未指定值。
;;
;; 说明
;; ----
;; show 是 SRFI-166 的唯一入口，所有组合子只有经 show
;; 求值才会产生输出；show 同时负责维护行列位置等格式化状态。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (liii base) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f "hi") => "hi")
(check (show #f "hello " "world") => "hello world")
(check (show #f "a" nothing "b") => "ab")

(check (call-with-output-string (lambda (out) (show out "to-port")))
  =>
  "to-port"
) ;check

(check-report)
