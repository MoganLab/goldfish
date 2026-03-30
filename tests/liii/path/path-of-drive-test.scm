(import (liii check)
        (liii path)
        (liii error)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-of-drive
;; 根据驱动器字符构造 Windows 风格的路径。
;;
;; 语法
;; ----
;; (path-of-drive ch)
;;
;; 参数
;; ----
;; ch : char?
;; 驱动器字母，如 #\\C 表示 C: 盘。
;;
;; 返回值
;; ----
;; path-value
;; 返回对应驱动器的根路径，如 C:\\\n;;
;; 示例
;; ----
;; (path->string (path-of-drive #\\C)) 返回 "C:\\\n;;
;; 错误处理
;; ----
;; type-error 当传入非字符参数时。

(check (path->string (path-of-drive #\C)) => "C:\\")
(check (path-type (path-of-drive #\C)) => 'windows)
(check (path-drive (path-root)) => "")
(check (path-drive (path-of-drive #\C)) => "C")

(check-catch 'type-error (path-of-drive 1))

(check-report)
