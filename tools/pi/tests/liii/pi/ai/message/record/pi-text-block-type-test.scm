;; pi-text-block-type
;; 返回文本块的固定类型字符串 "text"。
;;
;; 语法
;; ----
;; (pi-text-block-type block)
;;
;; 参数
;; ----
;; block : any?
;; 文本块对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "text"。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-text-block-type 'dummy) => "text")

(check-report)
