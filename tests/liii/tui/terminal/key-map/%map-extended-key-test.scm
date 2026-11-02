;; %map-extended-key
;; 把控制台的扩展键码（0/224 前缀后的第二个字节）映射为按键符号。
;;
;; 语法
;; ----
;; (%map-extended-key code)
;;
;; 参数
;; ----
;; code : integer?
;; 控制台扩展键码。
;;
;; 返回值
;; ----
;; symbol? 或 #f
;; 映射出的按键符号；无法识别时返回 #f。

(import (scheme base) (liii check) (liii tui terminal key-map))

(check-set-mode! 'report-failed)

;; 方向键与导航键
(check (%map-extended-key 72) => 'up)
(check (%map-extended-key 80) => 'down)
(check (%map-extended-key 75) => 'left)
(check (%map-extended-key 77) => 'right)
(check (%map-extended-key 71) => 'home)
(check (%map-extended-key 79) => 'end)
(check (%map-extended-key 73) => 'page-up)
(check (%map-extended-key 81) => 'page-down)
(check (%map-extended-key 83) => 'delete)
(check (%map-extended-key 82) => 'insert)

;; Ctrl 组合
(check (%map-extended-key 115) => 'ctrl-left)
(check (%map-extended-key 116) => 'ctrl-right)
(check (%map-extended-key 119) => 'ctrl-home)
(check (%map-extended-key 117) => 'ctrl-end)
(check (%map-extended-key 141) => 'ctrl-up)
(check (%map-extended-key 145) => 'ctrl-down)
(check (%map-extended-key 147) => 'ctrl-delete)

;; Alt 组合
(check (%map-extended-key 152) => 'alt-up)
(check (%map-extended-key 160) => 'alt-down)
(check (%map-extended-key 155) => 'alt-left)
(check (%map-extended-key 157) => 'alt-right)

;; Shift+Tab
(check (%map-extended-key 15) => 'shift-tab)
(check (%map-extended-key 353) => 'shift-tab)

;; 功能键 F1-F10 与 F11/F12
(check (%map-extended-key 59) => 'f1)
(check (%map-extended-key 68) => 'f10)
(check (%map-extended-key 133) => 'f11)
(check (%map-extended-key 134) => 'f12)

;; 无法识别返回 #f
(check (%map-extended-key 999) => #f)

(check-report)
