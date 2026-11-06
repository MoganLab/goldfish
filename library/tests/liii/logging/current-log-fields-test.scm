;; current-log-fields
;; 读取当前生效的全局日志字段（plist 形态）。
;;
;; 语法
;; ----
;; (current-log-fields)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list
;; 全局字段的 plist 形态；未设置时为空列表 '()。
;;
;; 说明
;; ----
;; 字段以 plist 形式保存，发送日志时与消息级字段合并为 alist。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(check (current-log-fields) => '())

(log-set-fields! '(APP "test"))
(check (current-log-fields) => '(APP "test"))

;; 恢复默认
(log-set-fields! '())
(check (current-log-fields) => '())

(check-report)
