(import (liii check)
        (liii enum))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham")))

(define pizza (make-enum-type pizza-descriptions))

;; enum-min
;; 获取 enum-type 中最小的 enum。
;;
;; 语法
;; ----
;; (enum-min enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; enum
;; 序数为 0 的 enum。
;;
;; 注意
;; ----
;; 返回第一个定义的 enum。
;;
;; 示例
;; ----
;; (enum-name (enum-min color)) => 'red
;;
;; 错误处理
;; ----
;; 无。

(check (enum-name (enum-min color)) => 'red)
(check (enum-name (enum-min pizza)) => 'margherita)

(check-report)
