;; pi-block-type
;; 获取内容块对象的类型名称。
;;
;; 语法
;; ----
;; (pi-block-type block)
;;
;; 参数
;; ----
;; block : any?
;; 内容块 record 或 alist。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 块类型字符串（如 "text"、"thinking"、"toolCall"）。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-block-type (pi-text-block "txt")) => "text")
(check (pi-block-type (pi-thinking-block "think")) => "thinking")

(check-report)
