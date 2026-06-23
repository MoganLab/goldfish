(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-type
;; 获取路径类型。
;;
;; 语法
;; ----
;; (path-type path-value)
;;
;; 参数
;; ----
;; path-value : path-value
;; 要查询的路径值。
;;
;; 返回值
;; ----
;; symbol
;; 返回 'posix 或 'windows。

(check (path-type (path)) => 'posix)
(when (not (os-windows?))
  ;; posix 平台:(path "/") 是 posix 根
  (check (path-type (path "/")) => 'posix)
) ;when
(when (os-windows?)
  ;; Windows 平台:(path "/") 被规范化为 windows "\"
  (check (path-type (path "/")) => 'windows)
) ;when
(check (path-type (path-of-drive #\c)) => 'windows)

(check-report)
