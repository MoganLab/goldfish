;; os-sep
;; 获取操作系统的目录分隔符。
;;
;; 语法
;; ----
;; (os-sep)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; char?
;; Windows 返回 #\\，其他平台返回 #\/。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 按平台断言分隔符
(if (os-windows?) (check (os-sep) => #\\) (check (os-sep) => #\/))

(check-true (char? (os-sep)))

(check-report)
