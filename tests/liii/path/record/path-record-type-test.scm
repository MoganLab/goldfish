;; path-record-type
;; 读取路径记录的类型字段。
;;
;; 语法
;; ----
;; (path-record-type p)
;;
;; 参数
;; ----
;; p : path?
;; 路径记录对象。
;;
;; 返回值
;; ----
;; symbol
;; 'posix 或 'windows。

(import (liii check) (liii path) (liii path record))

(check-set-mode! 'report-failed)

(check (path-record-type (path "/a/b")) => 'posix)
(check (path-record-type (make-path-record #("a") 'windows "C" #\\)) => 'windows)

(check-report)
