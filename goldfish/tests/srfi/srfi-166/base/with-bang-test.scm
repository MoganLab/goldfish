;; with!
;; 就地修改当前格式化环境的状态变量。
;;
;; 语法
;; ----
;; (with! (var val) ...)
;;
;; 参数
;; ----
;; var : symbol?
;; 状态变量名，如 pad-char、ellipsis、radix。
;;
;; val : any
;; 该变量的新值。
;;
;; 返回值
;; ----
;; format?
;; 一个只执行状态修改、不输出内容的组合子。
;;
;; 说明
;; ----
;; 与 with 的区别是 with! 不创建局部作用域，修改对后续
;; 组合子持续生效；读取变量值需要经 fn 的形参。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (with! (pad-char #\-)) (padded 4 "ab")) => "--ab")
(check (show #f (with!) (padded 4 "ab")) => "  ab")

(check-report)
