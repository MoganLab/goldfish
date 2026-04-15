(import (liii check)
  (liii enum)
  (srfi srfi-1)
) ;import


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


;; enum-type-values
;; 获取 enum-type 中所有值。
;;
;; 语法
;; ----
;; (enum-type-values enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; list
;; 按序数排序的值列表。
;;
;; 注意
;; ----
;; 若未显式指定值，则返回对应序数列表。
;;
;; 示例
;; ----
;; (enum-type-values pizza) => (map cadr pizza-descriptions)
;;
;; 错误处理
;; ----
;; 无。


(check (map cadr pizza-descriptions)
  =>
  (enum-type-values pizza)
) ;check
(check (iota (enum-type-size color))
  =>
  (enum-type-values color)
) ;check


(check-report)
