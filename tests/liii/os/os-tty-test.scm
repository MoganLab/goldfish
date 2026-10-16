(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; os-tty?
;; 检查文件描述符是否连接到终端。
;;
;; 语法
;; ----
;; (os-tty? fd)
;;
;; 参数
;; ----
;; fd : exact-integer?
;; 文件描述符（如 0: stdin, 1: stdout, 2: stderr）。
;;
;; 返回值
;; -----
;; boolean?
;; 如果文件描述符关联到终端设备返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; type-error
;; 当 fd 不是整数时抛出错误。
;;
;; 说明
;; ----
;; 底层调用 POSIX isatty 或 Windows _isatty。
;; 对于无效或已关闭的文件描述符（如负数），安全返回 #f，不引发异常。

;; 基本返回值类型校验
(check-true (boolean? (os-tty? 0)))
(check-true (boolean? (os-tty? 1)))
(check-true (boolean? (os-tty? 2)))

;; 无效文件描述符容错：返回 #f 而非报错或崩溃
(check-false (os-tty? -1))
(check-false (os-tty? 9999))
(check-false (os-tty? 1000000))

;; 参数类型错误校验
(check-catch 'type-error (os-tty? "0"))
(check-catch 'type-error (os-tty? 'stdin))
(check-catch 'type-error (os-tty? 1.5))
(check-catch 'type-error (os-tty? 1.0))
(check-catch 'type-error (os-tty? #t))
(check-catch 'type-error (os-tty? '()))
(check-catch 'type-error (os-tty? (list 0)))

;; C 胶水层直接调用校验
(check-catch 'type-error (g_os_tty_p "0"))
(check-catch 'type-error (g_os_tty_p 'stdin))
(check-catch 'type-error (g_os_tty_p 1.5))
(check-false (g_os_tty_p -1))
(check-false (g_os_tty_p 9999))

(check-report)
