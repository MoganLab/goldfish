;; pi-tool-call-block-type
;; 返回工具调用块的固定类型字符串 "toolCall"。
;;
;; 语法
;; ----
;; (pi-tool-call-block-type block)
;;
;; 参数
;; ----
;; block : any?
;; 工具调用块对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "toolCall"。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-tool-call-block-type 'dummy) => "toolCall")

(check-report)
