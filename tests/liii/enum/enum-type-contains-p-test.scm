(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham"))
) ;define

(define pizza (make-enum-type pizza-descriptions))

;; enum-type-contains?
;; 判断 enum 是否属于指定的 enum-type。
;;
;; 语法
;; ----
;; (enum-type-contains? enum-type enum)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; enum : enum?
;; 待检查的 enum。
;;
;; 返回值
;; ----
;; boolean
;; 属于该 enum-type 时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 不同 enum-type 之间的 enum 不会被视为成员。
;;
;; 示例
;; ----
;; (enum-type-contains? color color-red) => #t
;; (enum-type-contains? pizza color-red) => #f
;;
;; 错误处理
;; ----
;; 无。

(check (enum-type-contains? color (enum-name->enum color 'red)) => #t)
(check (enum-type-contains? pizza (enum-name->enum color 'red)) => #f)

(check-report)
