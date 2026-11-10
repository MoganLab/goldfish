;; GREEN
;; GREEN 终端颜色 ANSI 转义常量。
;;
;; 语法
;; ----
;; GREEN
;;
;; 参数
;; ----
;; 无（常量）。
;;
;; 返回值
;; ------
;; string?，ANSI 转义序列。

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; 常量断言：为字符串
(check-true (string? GREEN))

(check-report)
