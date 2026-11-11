;; pi-probe-prompt
;; 金丝雀预检使用的微型提示词常量。
;;
;; 语法
;; ----
;; pi-probe-prompt
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; string
;; 固定为 ping，不包含主会话消息。

(import (scheme base) (liii check) (liii pi code all-in-one probe))

(check-set-mode! 'report-failed)

(check pi-probe-prompt => "ping")

(check-report)
