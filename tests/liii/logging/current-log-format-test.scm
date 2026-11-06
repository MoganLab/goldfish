;; current-log-format
;; 读取当前日志格式串。
;;
;; 语法
;; ----
;; (current-log-format)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string
;; 当前格式串；缺省为 "%(asctime)s [%(levelname)s] %(message)s"。
;;
;; 说明
;; ----
;; 格式串使用 pyfmt 占位符，支持 asctime、levelname、severity、message。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(check (string? (current-log-format)) => #t)

(log-set-format! "[%(levelname)s] %(message)s")
(check (current-log-format) => "[%(levelname)s] %(message)s")

;; 恢复默认
(log-set-format! "%(asctime)s [%(levelname)s] %(message)s")
(check (current-log-format) => "%(asctime)s [%(levelname)s] %(message)s")

(check-report)
