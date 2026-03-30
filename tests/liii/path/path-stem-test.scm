(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-stem
;; 获取文件名主名（不含最后扩展名）。
;;
;; 语法
;; ----
;; (path-stem path-value) -> string
;;
;; 参数
;; ----
;; path-value : path-value
;; 路径值。
;;
;; 返回值
;; ----
;; string
;; 返回主文件名（不含最后扩展名）。
;;
;; 描述
;; ----
;; 对于多后缀文件，只去除最后一个扩展名。

(check (path-stem (path "file.txt")) => "file")
(check (path-stem (path "archive.tar.gz")) => "archive.tar")
(check (path-stem (path ".hidden")) => ".hidden")
(check (path-stem (path "noext")) => "noext")
(check (path-stem (path "")) => "")
(check (path-stem (path ".")) => "")
(check (path-stem (path "..")) => "..")
(check (path-stem (path "config.yaml.bak")) => "config.yaml")
(check (path-stem (path "test-file.name-with-dots.txt")) => "test-file.name-with-dots")

(check-report)
