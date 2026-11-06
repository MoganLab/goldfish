;; check-failed?
;; 判断当前上下文中是否存在至少一个失败的断言。
;;
;; 语法
;; ----
;; (check-failed?)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受任何参数。
;;
;; 返回值
;; ----
;; boolean
;; 若存在失败断言返回 #t，否则返回 #f。

(import (srfi srfi-78))

(check-set-mode! 'summary)

(check (check-failed?) => #f)

(check 1 => 2)

(check (check-failed?) => #t)

(check-reset!)

(check (check-failed?) => #f)

(check-report)
