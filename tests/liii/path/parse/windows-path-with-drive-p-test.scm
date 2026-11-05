;; windows-path-with-drive?
;; 判断字符串是否为带盘符的 Windows 路径。
;;
;; 语法
;; ----
;; (windows-path-with-drive? s)
;;
;; 参数
;; ----
;; s : string?
;; 待判断的路径字符串。
;;
;; 返回值
;; ----
;; boolean
;; 以字母加冒号开头（如 "C:"）返回 #t，否则 #f。

(import (liii check) (liii path parse))

(check-set-mode! 'report-failed)

(check (windows-path-with-drive? "C:\\a") => #t)
(check (windows-path-with-drive? "C:foo") => #t)
(check (windows-path-with-drive? "c:x") => #t)
(check (windows-path-with-drive? "/a") => #f)
(check (windows-path-with-drive? "C") => #f)
(check (windows-path-with-drive? "") => #f)

(check-report)
