(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-drive
;; 获取路径的驱动器字母。
;;
;; 语法
;; ----
;; (path-drive path-value)
;;
;; 参数
;; ----
;; path-value : path-value
;; 要查询的路径值。
;;
;; 返回值
;; ----
;; string
;; 返回驱动器字母，如 "C"；非 Windows 路径返回空字符串。

(check (path-drive (path-root)) => "")
(check (path-drive (path-of-drive #\c))
  =>
  "C"
) ;check

(check-report)
