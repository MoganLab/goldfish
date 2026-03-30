(import (liii check)
        (liii enum))

(check-set-mode! 'report-failed)

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham")))

(define pizza (make-enum-type pizza-descriptions))

(define pizza-chicago (enum-name->enum pizza 'chicago))

(define pizza-bianca (enum-name->enum pizza 'bianca))

;; list->enum-set
;; 从 enum 列表创建 enum-set。
;;
;; 语法
;; ----
;; (list->enum-set enum-type list)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; list : list?
;; 由 enum 组成的列表。
;;
;; 返回值
;; ----
;; enum-set
;; 由列表成员组成的新集合。
;;
;; 注意
;; ----
;; 输入列表中的 enum 需要属于同一 enum-type。
;;
;; 示例
;; ----
;; (enum-set-contains? (list->enum-set pizza (list pizza-chicago pizza-bianca)) pizza-chicago) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-contains? (list->enum-set pizza (list pizza-chicago pizza-bianca)) pizza-chicago) => #t)

(check-report)
