;; pathsep
;; 获取操作系统的路径列表分隔符。
;;
;; 语法
;; ----
;; (pathsep)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; char?
;; Windows 返回 #\;，其他平台返回 #\:。
;;
;; 说明
;; ----
;; 用于拼装 PATH 等路径列表环境变量。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 按平台断言分隔符
(if (os-windows?) (check (pathsep) => #\;) (check (pathsep) => #\:))

(check-true (char? (pathsep)))

(check-report)
