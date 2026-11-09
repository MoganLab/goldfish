;; space-to
;; 把输出位置推进到指定列的空白组合子。
;;
;; 语法
;; ----
;; (space-to where)
;;
;; 参数
;; ----
;; where : exact-integer?
;; 目标列号。
;;
;; 返回值
;; ----
;; format?
;; 输出空格使当前列号推进到目标列的组合子。
;;
;; 说明
;; ----
;; 当前列号已到达或超过目标列时不输出任何字符；
;; 同族的 tab-to 按制表位步进，nl 换行、fl 按需换行、
;; nothing 什么都不输出。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f "a" (space-to 5) "b") => "a    b")
(check (show #f "a" (space-to 0) "b") => "ab")
(check (show #f "abcdef" (space-to 3) "g") => "abcdefg")
(check (show #f "a" (space-to 1) "b") => "ab")

(check-report)
