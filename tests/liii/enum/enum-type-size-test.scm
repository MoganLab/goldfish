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

;; enum-type-size
;; 获取 enum-type 中 enum 的数量。
;;
;; 语法
;; ----
;; (enum-type-size enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; exact-integer
;; enum 的数量。
;;
;; 注意
;; ----
;; 结果等于名称列表长度。
;;
;; 示例
;; ----
;; (enum-type-size color) => 8
;;
;; 错误处理
;; ----
;; 无。

(check (enum-type-size color) => (length color-names))
(check (enum-type-size pizza) => (length pizza-names))

(check-report)
