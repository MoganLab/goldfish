(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-suffix
;; 获取文件扩展名。
;;
;; 语法
;; ----
;; (path-suffix path-value) -> string
;;
;; 参数
;; ----
;; path-value : path-value
;; 路径值。
;;
;; 返回值
;; ----
;; string
;; 返回文件扩展名（包含点），如果没有扩展名则返回空字符串。
;;
;; 描述
;; ----
;; 对于多后缀文件，只返回最后一个扩展名。
;; 隐藏文件（以点开头的文件名）没有扩展名。

(check (path-suffix (path "file.txt")) => ".txt")
(check (path-suffix (path "archive.tar.gz")) => ".gz")
(check (path-suffix (path ".hidden")) => "")
(check (path-suffix (path "noext")) => "")
(check (path-suffix (path "")) => "")
(check (path-suffix (path ".")) => "")
(check (path-suffix (path "..")) => "")
(check (path-suffix (path-join (path-of-drive #\C) "Users" "report.txt")) => ".txt")

(check-report)
