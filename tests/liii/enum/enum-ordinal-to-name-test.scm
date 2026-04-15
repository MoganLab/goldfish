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


;; enum-ordinal->name
;; 通过序数获取 enum 名称。
;;
;; 语法
;; ----
;; (enum-ordinal->name enum-type exact-integer)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; exact-integer : integer?
;; 目标序数。
;;
;; 返回值
;; ----
;; symbol
;; 对应的 enum 名称。
;;
;; 注意
;; ----
;; 示例覆盖颜色和披萨两类枚举。
;;
;; 示例
;; ----
;; (enum-ordinal->name color 0) => 'red
;;
;; 错误处理
;; ----
;; 原始测试未覆盖错误分支。


(check (enum-ordinal->name color 0)
  =>
  'red
) ;check
(check (enum-ordinal->name pizza 3)
  =>
  'chicago
) ;check


(check-report)
