;; path-record-drive
;; 读取路径记录的盘符字段。
;;
;; 语法
;; ----
;; (path-record-drive p)
;;
;; 参数
;; ----
;; p : path?
;; 路径记录对象。
;;
;; 返回值
;; ----
;; string?
;; 盘符字母或 UNC 主机名；posix 路径为空串。

(import (liii check) (liii path) (liii path record))

(check-set-mode! 'report-failed)

(check (path-record-drive (path "/a/b")) => "")
(check (path-record-drive (make-path-record #("a") 'windows "C" #\\)) => "C")

(check-report)
