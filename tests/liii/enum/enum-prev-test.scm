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

;; enum-prev
;; 获取同一 enum-type 中前一个 enum。
;;
;; 语法
;; ----
;; (enum-prev enum)
;;
;; 参数
;; ----
;; enum : enum?
;; 目标 enum。
;;
;; 返回值
;; ----
;; enum or #f
;; 存在前驱时返回上一个 enum，否则返回 #f。
;;
;; 注意
;; ----
;; 对最小元素调用会得到 #f。
;;
;; 示例
;; ----
;; (enum=? (enum-prev color-tangerine) color-red) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum=? (enum-prev color-tangerine) color-red) => #t)
(check (enum=? (enum-prev pizza-chicago) pizza-bianca) => #t)
(check (enum-prev (enum-min color)) => #f)

(check-report)
