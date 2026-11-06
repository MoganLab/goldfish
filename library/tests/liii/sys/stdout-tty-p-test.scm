;; stdout-tty?
;; 检测标准输出流（文件描述符 1）是否连接到终端。
;;
;; 语法
;; ----
;; (stdout-tty?)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; -----
;; boolean?
;; 若标准输出为终端返回 #t，处于重定向或管道输出时返回 #f。
;;
;; 说明
;; ----
;; 顶层便捷谓词，等价于 (os-tty? 1)。

(import (liii check)
        (liii os)
        (liii sys))

(check-set-mode! 'report-failed)

;; 基本返回值类型校验
(check-true (boolean? (stdout-tty?)))

;; 与 (os-tty? fd) 行为一致性校验
(check (stdout-tty?) => (os-tty? 1))

;; 参数个数校验（无参函数）
(check-catch 'wrong-number-of-args (stdout-tty? 1))

(check-report)
