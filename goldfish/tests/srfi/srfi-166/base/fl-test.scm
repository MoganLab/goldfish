;; fl
;; 条件换行组合子：当前列不为 0 时才输出换行。
;;
;; 语法
;; ----
;; fl
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; format?
;; 视当前列号决定是否输出换行符的组合子。
;;
;; 说明
;; ----
;; 用于「保证独占一行」的收尾，重复使用不会产生多余空行；
;; 无条件换行请用 nl。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f "a" fl "b") => "a\nb")
(check (show #f "a" nl fl "b") => "a\nb")
(check (show #f fl) => "")

(check-report)
