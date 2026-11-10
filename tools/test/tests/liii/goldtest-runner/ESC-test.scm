;; ESC
;; ESC 终端转义前缀常量。
;;
;; 语法
;; ----
;; ESC
;;
;; 参数
;; ----
;; 无（常量）。
;;
;; 返回值
;; ------
;; string?，转义前缀。

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; 常量断言
(check-true (string? ESC))

(check-report)
