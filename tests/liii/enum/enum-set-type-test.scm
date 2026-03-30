(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham"))
) ;define

(define pizza (make-enum-type pizza-descriptions))

;; enum-set-type
;; 获取 enum-set 所属的 enum-type。
;;
;; 语法
;; ----
;; (enum-set-type enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; enum-type
;; 集合对应的枚举类型。
;;
;; 注意
;; ----
;; 返回值与创建该集合时使用的类型一致。
;;
;; 示例
;; ----
;; (eqv? (enum-set-type color-set) color) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (eqv? (enum-set-type color-set) color) => #t)
(check (eqv? (enum-set-type (enum-type->enum-set pizza)) pizza) => #t)

(check-report)
