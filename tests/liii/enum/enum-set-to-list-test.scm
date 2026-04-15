(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


(define color-set
  (enum-type->enum-set color)
) ;define


(define pizza-descriptions
  '((margherita "tomato and mozzarella") (funghi "mushrooms") (bianca "ricotta and mozzarella") (chicago "deep-dish") (hawaiian "pineapple and ham"))
) ;define


(define pizza
  (make-enum-type pizza-descriptions)
) ;define


;; enum-set->list
;; 将 enum-set 转换为名称列表。
;;
;; 语法
;; ----
;; (enum-set->list enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; list
;; 按序数排序的名称列表。
;;
;; 注意
;; ----
;; 结果中的元素是符号。
;;
;; 示例
;; ----
;; (enum-set->list color-set) => color-names
;;
;; 错误处理
;; ----
;; 无。


(check color-names
  =>
  (enum-set->list color-set)
) ;check
(check (map car pizza-descriptions)
  =>
  (enum-set->list (enum-type->enum-set pizza)
  ) ;enum-set->list
) ;check


(check-report)
