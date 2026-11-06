;; argv
;; 返回当前进程的命令行参数列表。
;;
;; 语法
;; ----
;; (argv)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list
;; 命令行参数字符串列表，第一个元素为程序名。
;;
;; 说明
;; ----
;; 等价于 (scheme process-context) 的 command-line。

(import (liii check)
        (liii list)
        (liii sys))

(check-set-mode! 'report-failed)

(check-true (list? (argv)))
(check-true (pair? (argv)))
(check-true (string? (car (argv))))
(check-true (every string? (argv)))

;; 参数个数校验（无参函数）
(check-catch 'wrong-number-of-args (argv 1))

(check-report)
