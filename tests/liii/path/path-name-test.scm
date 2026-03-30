(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-name
;; 获取文件名。
;;
;; 语法
;; ----
;; (path-name path-value) -> string
;;
;; 参数
;; ----
;; path-value : path-value
;; 路径值。
;;
;; 返回值
;; ----
;; string
;; 返回文件名部分。
;;
;; 描述
;; ----
;; 获取路径的文件名部分，适用于普通文件名、隐藏文件、
;; 多后缀文件以及绝对/相对路径。

(check (path-name (path "file.txt")) => "file.txt")
(check (path-name (path "archive.tar.gz")) => "archive.tar.gz")
(check (path-name (path ".hidden")) => ".hidden")
(check (path-name (path "noext")) => "noext")
(check (path-name (path "")) => "")
(check (path-name (path ".")) => "")
(check (path-name (path "..")) => "..")

(when (not (os-windows?))
  (check (path-name (path "/path/to/file.txt")) => "file.txt")
) ;when

(check (path-name (path-join (path-of-drive #\C) "Users" "report.txt")) => "report.txt")

(check-report)
