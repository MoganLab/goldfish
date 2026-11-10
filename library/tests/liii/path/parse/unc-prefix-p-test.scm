;; unc-prefix?
;; 判断字符串是否以 UNC 前缀（\\）开头。
;;
;; 语法
;; ----
;; (unc-prefix? s)
;;
;; 参数
;; ----
;; s : string?
;; 待判断的路径字符串。
;;
;; 返回值
;; ----
;; boolean
;; 以连续两个反斜杠开头返回 #t，否则 #f。

(import (liii check) (liii path parse))

(check-set-mode! 'report-failed)

(check (unc-prefix? "\\\\server") => #t)
(check (unc-prefix? "\\\\server\\share") => #t)
(check (unc-prefix? "\\server") => #f)
(check (unc-prefix? "/server") => #f)
(check (unc-prefix? "") => #f)

(check-report)
