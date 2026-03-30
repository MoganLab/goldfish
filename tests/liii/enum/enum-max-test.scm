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

(define pizza (make-enum-type pizza-descriptions))

;; enum-max
;; 获取 enum-type 中最大的 enum。
;;
;; 语法
;; ----
;; (enum-max enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; enum
;; 序数最大的 enum。
;;
;; 注意
;; ----
;; 返回最后一个定义的 enum。
;;
;; 示例
;; ----
;; (enum-name (enum-max color)) => 'violet
;;
;; 错误处理
;; ----
;; 无。

(check (enum-name (enum-max color)) => 'violet)
(check (enum-name (enum-max pizza)) => 'hawaiian)

(check-report)
