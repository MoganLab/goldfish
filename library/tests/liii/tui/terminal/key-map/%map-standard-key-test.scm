;; %map-standard-key
;; 把标准控制台输入的字节码映射为按键符号或字符串。
;;
;; 语法
;; ----
;; (%map-standard-key code)
;;
;; 参数
;; ----
;; code : integer?
;; 控制台输入的字节码。
;;
;; 返回值
;; ----
;; any
;; 按键符号（如 'enter）或单字符字符串。

(import (scheme base) (liii check) (liii tui terminal key-map))

(check-set-mode! 'report-failed)

;; 控制字符映射
(check (%map-standard-key 13) => 'enter)
(check (%map-standard-key 10) => 'ctrl-j)
(check (%map-standard-key 27) => 'escape)
(check (%map-standard-key 8) => 'backspace)
(check (%map-standard-key 127) => 'backspace)
(check (%map-standard-key 9) => 'tab)
(check (%map-standard-key 3) => 'ctrl-c)
(check (%map-standard-key 4) => 'ctrl-d)

;; 扩展键码
(check (%map-standard-key 353) => 'shift-tab)
(check (%map-standard-key 354) => 'shift-enter)
(check (%map-standard-key 355) => 'alt-enter)
(check (%map-standard-key 356) => 'ctrl-enter)

;; 可打印字符返回单字符字符串
(check (%map-standard-key 97) => "a")
(check (%map-standard-key 32) => " ")
(check (%map-standard-key 126) => "~")

(check-report)
