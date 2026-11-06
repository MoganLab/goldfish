;; current-log-level
;; 读取当前日志级别阈值。
;;
;; 语法
;; ----
;; (current-log-level)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; exact-integer?
;; 当前级别阈值；缺省为 WARNING（4）。
;;
;; 说明
;; ----
;; 严重程度数值大于等于该阈值的消息才会被发送。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

;; 缺省为 WARNING（与 Python logging 对齐）
(check (current-log-level) => WARNING)

(log-set-level! DEBUG)
(check (current-log-level) => DEBUG)

;; 恢复默认
(log-set-level! WARNING)
(check (current-log-level) => WARNING)

(check-report)
