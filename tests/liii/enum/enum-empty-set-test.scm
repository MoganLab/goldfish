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

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham"))
) ;define

(define pizza (make-enum-type pizza-descriptions))

;; enum-empty-set
;; 创建空的 enum-set。
;;
;; 语法
;; ----
;; (enum-empty-set enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; enum-set
;; 不含任何成员的枚举集合。
;;
;; 注意
;; ----
;; 返回的集合仍然绑定到原 enum-type。
;;
;; 示例
;; ----
;; (enum-set-empty? (enum-empty-set pizza)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-empty? (enum-empty-set pizza)) => #t)
(check (enum-set-empty? empty-colors) => #t)
(check (enum-set-empty? color-set) => #f)

(check-report)
