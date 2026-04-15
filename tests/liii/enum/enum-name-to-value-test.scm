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


;; enum-name->value
;; 通过名称获取 enum 的值。
;;
;; 语法
;; ----
;; (enum-name->value enum-type symbol)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; symbol : symbol?
;; enum 名称。
;;
;; 返回值
;; ----
;; any?
;; 目标 enum 的值。
;;
;; 注意
;; ----
;; 对于未显式绑定值的 enum，返回其序数。
;;
;; 示例
;; ----
;; (enum-name->value pizza 'funghi) => "mushrooms"
;;
;; 错误处理
;; ----
;; 原始测试未覆盖错误分支。


(check (enum-name->value pizza 'funghi)
  =>
  "mushrooms"
) ;check
(check (enum-name->value color 'blue)
  =>
  6
) ;check


(check-report)
