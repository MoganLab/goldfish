(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham"))
) ;define

(define pizza-names (map car pizza-descriptions))

(define pizza (make-enum-type pizza-descriptions))

;; enum-type-names
;; 获取 enum-type 中所有名称。
;;
;; 语法
;; ----
;; (enum-type-names enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; list
;; 按序数排序的符号列表。
;;
;; 注意
;; ----
;; 结果可直接与原始名称列表比较。
;;
;; 示例
;; ----
;; (enum-type-names color) => color-names
;;
;; 错误处理
;; ----
;; 无。

(check (enum-type-names color) => color-names)
(check (enum-type-names pizza) => pizza-names)

(check-report)
