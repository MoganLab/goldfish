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

;; enum-set-map->list
;; 对 enum-set 中每个成员应用过程并收集结果。
;;
;; 语法
;; ----
;; (enum-set-map->list proc enum-set)
;;
;; 参数
;; ----
;; proc : procedure?
;; 映射过程。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; list
;; 映射结果列表。
;;
;; 注意
;; ----
;; 遍历顺序按序数排列。
;;
;; 示例
;; ----
;; (enum-set-map->list enum-name color-set) => color-names
;;
;; 错误处理
;; ----
;; 无。

(check color-names => (enum-set-map->list enum-name color-set))
(check (null? (enum-set-map->list enum-name empty-colors)) => #t)

(check-report)
