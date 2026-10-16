(import (liii check)
        (liii sys)
        (liii os))

(check-set-mode! 'report-failed)

;; stdin-tty?
;; 检测标准输入流（文件描述符 0）是否连接到终端。
;;
;; 语法
;; ----
;; (stdin-tty?)
;;
;; 返回值
;; -----
;; boolean?
;; 若标准输入为终端返回 #t，处于重定向或管道输入时返回 #f。
;;
;; 说明
;; ----
;; 顶层便捷谓词，等价于 (os-tty? 0)。

;; 基本返回值类型校验
(check-true (boolean? (stdin-tty?)))
(check-true (boolean? (stdout-tty?)))
(check-true (boolean? (stderr-tty?)))

;; 与 (os-tty? fd) 行为一致性校验
(check (stdin-tty?) => (os-tty? 0))
(check (stdout-tty?) => (os-tty? 1))
(check (stderr-tty?) => (os-tty? 2))

;; 参数个数校验（无参函数）
(check-catch 'wrong-number-of-args (stdin-tty? 0))
(check-catch 'wrong-number-of-args (stdout-tty? 1))
(check-catch 'wrong-number-of-args (stderr-tty? 2))

(check-report)
