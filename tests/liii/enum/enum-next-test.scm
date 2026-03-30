(import (liii check)
        (liii enum))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define color-tangerine (enum-name->enum color 'tangerine))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham")))

(define pizza (make-enum-type pizza-descriptions))

(define pizza-chicago (enum-name->enum pizza 'chicago))

(define pizza-bianca (enum-name->enum pizza 'bianca))

;; enum-next
;; 获取同一 enum-type 中下一个 enum。
;;
;; 语法
;; ----
;; (enum-next enum)
;;
;; 参数
;; ----
;; enum : enum?
;; 目标 enum。
;;
;; 返回值
;; ----
;; enum or #f
;; 存在后继时返回下一个 enum，否则返回 #f。
;;
;; 注意
;; ----
;; 对最大元素调用会得到 #f。
;;
;; 示例
;; ----
;; (enum=? (enum-next color-red) color-tangerine) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum=? (enum-next color-red) color-tangerine) => #t)
(check (enum=? (enum-next pizza-bianca) pizza-chicago) => #t)
(check (enum-next (enum-max color)) => #f)

(check-report)
