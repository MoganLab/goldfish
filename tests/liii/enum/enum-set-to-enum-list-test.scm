(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define empty-colors (enum-empty-set color))

;; enum-set->enum-list
;; 将 enum-set 转换为 enum 列表。
;;
;; 语法
;; ----
;; (enum-set->enum-list enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; list
;; 按序数排序的 enum 列表。
;;
;; 注意
;; ----
;; 空集合会返回空列表。
;;
;; 示例
;; ----
;; (enum-set->enum-list color-set) => (enum-type-enums color)
;;
;; 错误处理
;; ----
;; 无。

(check (enum-type-enums color) => (enum-set->enum-list color-set))
(check (null? (enum-set->enum-list empty-colors)) => #t)

(check-report)
