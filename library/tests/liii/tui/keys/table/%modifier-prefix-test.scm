;; %modifier-prefix
;; 将 CSI 序列中的修饰键代码转换为键名前缀字符串。
;;
;; 语法
;; ----
;; (%modifier-prefix mod-code)
;;
;; 参数
;; ----
;; mod-code : integer?，CSI 参数中的修饰键代码（2 到 8）
;;
;; 返回值
;; ----
;; 已知修饰键代码返回对应前缀字符串（如 "shift-"、"ctrl-"），
;; 无修饰或未知代码返回空字符串。

(import (liii check) (liii tui keys table))

(check-set-mode! 'report-failed)

;; 单修饰键
(check (%modifier-prefix 2) => "shift-")
(check (%modifier-prefix 3) => "alt-")
(check (%modifier-prefix 5) => "ctrl-")

;; 双修饰键组合
(check (%modifier-prefix 4) => "shift-alt-")
(check (%modifier-prefix 6) => "shift-ctrl-")
(check (%modifier-prefix 7) => "alt-ctrl-")

;; 三修饰键组合
(check (%modifier-prefix 8) => "shift-alt-ctrl-")

;; 无修饰或未知代码
(check (%modifier-prefix 1) => "")
(check (%modifier-prefix 0) => "")
(check (%modifier-prefix 9) => "")

(check-report)
