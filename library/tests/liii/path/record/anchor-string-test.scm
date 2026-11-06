;; anchor-string
;; 构造路径 anchor 首元素字符串。
;;
;; 语法
;; ----
;; (anchor-string type drive root)
;;
;; 参数
;; ----
;; type : symbol
;; 路径类型，'posix 或 'windows。
;; drive : string?
;; 盘符或 UNC 主机名。
;; root : char 或 #f
;; 根分隔符标记。
;;
;; 返回值
;; ----
;; string 或 #f
;; posix 绝对路径返回 "/"；Windows 返回 "C:\\"、"C:"、
;; "\\\\server\\share\\" 或 "\\"；纯相对路径返回 #f。

(import (liii check) (liii path record))

(check-set-mode! 'report-failed)

;; posix
(check (anchor-string 'posix "" #\/) => "/")
(check (anchor-string 'posix "" #f) => #f)

;; Windows 盘符
(check (anchor-string 'windows "C" #\\) => "C:\\")
(check (anchor-string 'windows "C" #f) => "C:")

;; Windows UNC
(check (anchor-string 'windows "\\\\server\\share" #\\)
  =>
  "\\\\server\\share\\"
) ;check

;; 当前盘根
(check (anchor-string 'windows "" #\\) => "\\")

(check-report)
