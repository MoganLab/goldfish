;; stderr-tty?
;; 检测标准错误流（文件描述符 2）是否连接到终端。
;;
;; 语法
;; ----
;; (stderr-tty?)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; -----
;; boolean?
;; 若标准错误为终端返回 #t，处于重定向或管道输出时返回 #f。
;;
;; 说明
;; ----
;; 顶层便捷谓词，等价于 (os-tty? 2)。

(import (liii check)
        (liii os)
        (liii sys))

(check-set-mode! 'report-failed)

;; 基本返回值类型校验
(check-true (boolean? (stderr-tty?)))

;; 与 (os-tty? fd) 行为一致性校验
(check (stderr-tty?) => (os-tty? 2))

;; 参数个数校验（无参函数）
(check-catch 'wrong-number-of-args (stderr-tty? 2))

(check-report)
