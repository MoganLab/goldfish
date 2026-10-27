(import (scheme base) (liii check) (liii pi code modes interactive thinking))

(check-set-mode! 'report-failed)

;; pi-next-thinking-level
;; 返回思考级别序列中的下一个级别，末尾循环回第一个级别。
;;
;; 语法
;; ----
;; (pi-next-thinking-level level [levels]) -> string
;;
;; 参数
;; ----
;; level : string
;; 当前级别；不在 levels 中时按未知级别处理。
;; levels : list
;; 可选的级别字符串列表，默认为 pi-thinking-levels。
;;
;; 返回值
;; ----
;; string
;; 下一个级别；未知级别或空 levels 返回 "off"。
;;
;; 示例
;; ----
;; (pi-next-thinking-level "high") => "xhigh"
;; (pi-next-thinking-level "max") => "off"

;; 按 7 级全集循环，非法输入容错回 "off"
(check (pi-next-thinking-level "off") => "minimal")
(check (pi-next-thinking-level "minimal") => "low")
(check (pi-next-thinking-level "high") => "xhigh")
(check (pi-next-thinking-level "max") => "off")
(check (pi-next-thinking-level "bogus") => "off")

(check-report)
