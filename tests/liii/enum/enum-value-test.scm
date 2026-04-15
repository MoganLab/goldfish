(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


(define pizza-descriptions
  '((margherita "tomato and mozzarella") (funghi "mushrooms") (bianca "ricotta and mozzarella") (chicago "deep-dish") (hawaiian "pineapple and ham"))
) ;define


(define pizza
  (make-enum-type pizza-descriptions)
) ;define


;; enum-value
;; 获取 enum 的值。
;;
;; 语法
;; ----
;; (enum-value enum)
;;
;; 参数
;; ----
;; enum : enum?
;; 目标 enum。
;;
;; 返回值
;; ----
;; any?
;; 定义时绑定的值；未指定时为序数。
;;
;; 注意
;; ----
;; 对于仅由名称构成的 enum-type，值默认为序数。
;;
;; 示例
;; ----
;; (enum-value (enum-name->enum pizza 'funghi)) => "mushrooms"
;;
;; 错误处理
;; ----
;; 无。


(check (enum-value (enum-name->enum pizza 'funghi)
       ) ;enum-value
  =>
  "mushrooms"
) ;check
(check (enum-value (enum-name->enum color 'blue)
       ) ;enum-value
  =>
  6
) ;check


(check-report)
