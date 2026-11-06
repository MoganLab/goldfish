;; %tilde-key
;; 将 CSI ~ 序列的数字参数转换为按键名。
;;
;; 语法
;; ----
;; (%tilde-key num-str)
;;
;; 参数
;; ----
;; num-str : string?，序列中 ~ 前的数字参数字符串
;;
;; 返回值
;; ----
;; 已知数字返回对应按键符号（home、insert、f1 等），
;; 未知数字返回 #f。

(import (liii check) (liii tui keys table))

(check-set-mode! 'report-failed)

;; 导航与编辑键
(check (%tilde-key "1") => 'home)
(check (%tilde-key "2") => 'insert)
(check (%tilde-key "3") => 'delete)
(check (%tilde-key "4") => 'end)
(check (%tilde-key "5") => 'page-up)
(check (%tilde-key "6") => 'page-down)
(check (%tilde-key "7") => 'home)
(check (%tilde-key "8") => 'end)

;; 功能键 F1 ~ F12
(check (%tilde-key "11") => 'f1)
(check (%tilde-key "12") => 'f2)
(check (%tilde-key "13") => 'f3)
(check (%tilde-key "14") => 'f4)
(check (%tilde-key "15") => 'f5)
(check (%tilde-key "17") => 'f6)
(check (%tilde-key "18") => 'f7)
(check (%tilde-key "19") => 'f8)
(check (%tilde-key "20") => 'f9)
(check (%tilde-key "21") => 'f10)
(check (%tilde-key "23") => 'f11)
(check (%tilde-key "24") => 'f12)

;; 未定义的数字
(check (%tilde-key "9") => #f)
(check (%tilde-key "10") => #f)
(check (%tilde-key "16") => #f)
(check (%tilde-key "22") => #f)
(check (%tilde-key "25") => #f)

(check-report)
