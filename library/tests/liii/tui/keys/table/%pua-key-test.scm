;; %pua-key
;; 将 kitty 键盘协议的功能键 PUA 码位转换为按键名。
;;
;; 语法
;; ----
;; (%pua-key code)
;;
;; 参数
;; ----
;; code : integer?，功能键的 Unicode 私有使用区码位值
;;
;; 返回值
;; ----
;; 已知功能键返回按键符号（up、down、f1 等），
;; 未知码位返回 #f。

(import (liii check) (liii tui keys table))

(check-set-mode! 'report-failed)

;; 编辑与方向键
(check (%pua-key 57348) => 'insert)
(check (%pua-key 57349) => 'delete)
(check (%pua-key 57350) => 'left)
(check (%pua-key 57351) => 'right)
(check (%pua-key 57352) => 'up)
(check (%pua-key 57353) => 'down)
(check (%pua-key 57354) => 'page-up)
(check (%pua-key 57355) => 'page-down)
(check (%pua-key 57356) => 'home)
(check (%pua-key 57357) => 'end)

;; 功能键 F1 至 F35
(check (%pua-key 57364) => 'f1)
(check (%pua-key 57365) => 'f2)
(check (%pua-key 57376) => 'f13)
(check (%pua-key 57398) => 'f35)

;; 不进表的控制码与未知码位返回 #f
(check (%pua-key 57344) => #f)
(check (%pua-key 57345) => #f)
(check (%pua-key 57346) => #f)
(check (%pua-key 57347) => #f)
(check (%pua-key 57363) => #f)
(check (%pua-key 57399) => #f)
(check (%pua-key 100) => #f)

(check-report)
