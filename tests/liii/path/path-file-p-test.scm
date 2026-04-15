(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-file?
;; 判断给定路径是否为文件。
;;
;; 语法
;; ----
;; (path-file? path)
;;
;; 参数
;; ----
;; path : string? | path-value
;; 文件路径。
;;
;; 返回值
;; ----
;; boolean
;; 当路径存在且为文件时返回 #t，否则返回 #f。

;; 基本功能测试
(check (path-file? ".") => #f)
(check (path-file? "..") => #f)
(check (path-file? "") => #f)
(check (path-file? "nonexistent") => #f)

(when (not (os-windows?))
  (check (path-file? "/") => #f)
  (check-true (path-file? "/etc/hosts"))
  (check (path-file? "/tmp") => #f)
) ;when

(when (os-windows?)
  (check (path-file? "C:/") => #f)
  (check-true (path-file? "C:/Windows/System32/drivers/etc/hosts"
              ) ;path-file?
  ) ;check-true
  (check (path-file? "C:/Windows") => #f)
) ;when

(check-report)
