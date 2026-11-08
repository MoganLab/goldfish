;; tab-to
;; 制表位组合子：把输出位置推进到下一个制表位。
;;
;; 语法
;; ----
;; (tab-to [tab-width])
;;
;; 参数
;; ----
;; tab-width : exact-integer?
;; 可选，制表位间隔，默认取状态变量 tab-width（8）。
;;
;; 返回值
;; ----
;; format?
;; 输出若干空格使列号落在下一个制表位的组合子。
;;
;; 说明
;; ----
;; 需要直接跳到绝对列号时请用 space-to。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f "abc" (tab-to) "def") => "abc     def")
(check (show #f "abc" (tab-to 5) "def") => "abc  def")
(check (show #f "abc" (tab-to 3) "def") => "abcdef")

(check-report)
