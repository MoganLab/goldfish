;; extract-drive
;; 提取 Windows 路径字符串的盘符字母。
;;
;; 语法
;; ----
;; (extract-drive s)
;;
;; 参数
;; ----
;; s : string?
;; 以盘符开头的 Windows 路径字符串。
;;
;; 返回值
;; ----
;; string
;; 大写的盘符字母（不含冒号）。

(import (liii check) (liii path parse))

(check-set-mode! 'report-failed)

(check (extract-drive "C:\\a") => "C")
(check (extract-drive "c:x") => "C")
(check (extract-drive "D:") => "D")

(check-report)
