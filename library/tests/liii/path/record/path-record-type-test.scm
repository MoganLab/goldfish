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

(import (liii check) (liii path) (liii path record) (liii os))

(check-set-mode! 'report-failed)

;; 无 drive/UNC 锚点时，type 取当前平台默认类型
(when (not (os-windows?))
  (check (path-record-type (path "/a/b")) => 'posix)
) ;when

(when (os-windows?)
  (check (path-record-type (path "C:\\a\\b")) => 'windows)
) ;when

(check (path-record-type (make-path-record #("a") 'windows "C" #\\)) => 'windows)

(check-report)
