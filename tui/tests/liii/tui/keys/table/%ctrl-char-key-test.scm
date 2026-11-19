;; %ctrl-char-key
;; 将 ASCII 控制字节转换为控制键名。
;;
;; 语法
;; ----
;; (%ctrl-char-key byte)
;;
;; 参数
;; ----
;; byte : integer?，0 到 31 以及 127 的 ASCII 字节值
;;
;; 返回值
;; ----
;; 已知控制字节返回按键符号（ctrl-a、enter、tab 等），
;; 可打印字节返回 #f。

(import (liii check) (liii tui keys table))

(check-set-mode! 'report-failed)

;; NUL 与 Ctrl+A 到 Ctrl+Z
(check (%ctrl-char-key 0) => 'ctrl-space)
(check (%ctrl-char-key 1) => 'ctrl-a)
(check (%ctrl-char-key 2) => 'ctrl-b)
(check (%ctrl-char-key 3) => 'ctrl-c)
(check (%ctrl-char-key 4) => 'ctrl-d)
(check (%ctrl-char-key 5) => 'ctrl-e)
(check (%ctrl-char-key 6) => 'ctrl-f)
(check (%ctrl-char-key 7) => 'ctrl-g)
(check (%ctrl-char-key 8) => 'backspace)
(check (%ctrl-char-key 9) => 'tab)
(check (%ctrl-char-key 10) => 'ctrl-j)
(check (%ctrl-char-key 11) => 'ctrl-k)
(check (%ctrl-char-key 12) => 'ctrl-l)
(check (%ctrl-char-key 13) => 'enter)
(check (%ctrl-char-key 14) => 'ctrl-n)
(check (%ctrl-char-key 15) => 'ctrl-o)
(check (%ctrl-char-key 16) => 'ctrl-p)
(check (%ctrl-char-key 17) => 'ctrl-q)
(check (%ctrl-char-key 18) => 'ctrl-r)
(check (%ctrl-char-key 19) => 'ctrl-s)
(check (%ctrl-char-key 20) => 'ctrl-t)
(check (%ctrl-char-key 21) => 'ctrl-u)
(check (%ctrl-char-key 22) => 'ctrl-v)
(check (%ctrl-char-key 23) => 'ctrl-w)
(check (%ctrl-char-key 24) => 'ctrl-x)
(check (%ctrl-char-key 25) => 'ctrl-y)
(check (%ctrl-char-key 26) => 'ctrl-z)
(check (%ctrl-char-key 27) => 'escape)

;; Ctrl+\、Ctrl+]、Ctrl+^、Ctrl+_
(check (%ctrl-char-key 28) => 'ctrl-\)
(check (%ctrl-char-key 29) => 'ctrl-])
(check (%ctrl-char-key 30) => 'ctrl-^)
(check (%ctrl-char-key 31) => 'ctrl-_)

;; DEL
(check (%ctrl-char-key 127) => 'backspace)

;; 可打印字节
(check (%ctrl-char-key 32) => #f)
(check (%ctrl-char-key 65) => #f)
(check (%ctrl-char-key 97) => #f)
(check (%ctrl-char-key 126) => #f)

(check-report)
