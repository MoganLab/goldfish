;; pi-thinking-block-type
;; 返回思考块的固定类型字符串 "thinking"。
;;
;; 语法
;; ----
;; (pi-thinking-block-type block)
;;
;; 参数
;; ----
;; block : any?
;; 思考块对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "thinking"。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-thinking-block-type 'dummy) => "thinking")

(check-report)
