(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham"))
) ;define

(define pizza (make-enum-type pizza-descriptions))

;; enum-ordinal->value
;; 通过序数获取 enum 值。
;;
;; 语法
;; ----
;; (enum-ordinal->value enum-type exact-integer)
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
;; any?
;; 对应序数位置 enum 的值。
;;
;; 注意
;; ----
;; 对带自定义值的 enum-type 很有用。
;;
;; 示例
;; ----
;; (enum-ordinal->value pizza 1) => "mushrooms"
;;
;; 错误处理
;; ----
;; 原始测试未覆盖错误分支。

(check (enum-ordinal->value pizza 1) => "mushrooms")

(check-report)
