;; fill-min-children
;; 使用 fill 布局时至少保留的子节点数量。
;;
;; 语法
;; ----
;; fill-min-children
;;
;; 参数
;; ----
;; 无
;; 该值为常量，不接受任何参数。
;;
;; 返回值
;; ----
;; integer?
;; 当前默认值为 4。
;;
;; 说明
;; ----
;; 该值用于 fill 布局的最小子节点数量判定。

(import (liii check) (liii goldfmt format atom))

(check-set-mode! 'report-failed)

(check fill-min-children => 4)

(check-report)
