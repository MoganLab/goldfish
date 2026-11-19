;; %ss3-key
;; 将 SS3 (ESC O X) 序列的终结字节转换为按键名。
;;
;; 语法
;; ----
;; (%ss3-key char-byte)
;;
;; 参数
;; ----
;; char-byte : integer?，SS3 序列的终结字节值
;;
;; 返回值
;; ----
;; 已知终结字节返回按键符号（up、home、f1 等），
;; 未知终结字节返回 #f。

(import (liii check) (liii tui keys table))

(check-set-mode! 'report-failed)

;; 方向键
(check (%ss3-key 65) => 'up)
(check (%ss3-key 66) => 'down)
(check (%ss3-key 67) => 'right)
(check (%ss3-key 68) => 'left)

;; 导航与编辑键
(check (%ss3-key 69) => 'clear)
(check (%ss3-key 70) => 'end)
(check (%ss3-key 72) => 'home)
(check (%ss3-key 77) => 'shift-enter)

;; 功能键 F1 ~ F4
(check (%ss3-key 80) => 'f1)
(check (%ss3-key 81) => 'f2)
(check (%ss3-key 82) => 'f3)
(check (%ss3-key 83) => 'f4)

;; 未知终结字节
(check (%ss3-key 64) => #f)
(check (%ss3-key 71) => #f)
(check (%ss3-key 84) => #f)
(check (%ss3-key 97) => #f)

(check-report)
