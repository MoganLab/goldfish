;; stdin-tty?
;; 检测标准输入流（文件描述符 0）是否连接到终端。
;;
;; 语法
;; ----
;; (stdin-tty?)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; -----
;; boolean?
;; 若标准输入为终端返回 #t，处于重定向或管道输入时返回 #f。
;;
;; 说明
;; ----
;; 顶层便捷谓词，等价于 (os-tty? 0)。

(import (liii check)
        (liii os)
        (liii sys))

(check-set-mode! 'report-failed)

;; 基本返回值类型校验
(check-true (boolean? (stdin-tty?)))

;; 与 (os-tty? fd) 行为一致性校验
(check (stdin-tty?) => (os-tty? 0))

;; 参数个数校验（无参函数）
(check-catch 'wrong-number-of-args (stdin-tty? 0))

(check-report)
