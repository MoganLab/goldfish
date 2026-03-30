(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-blue (enum-name->enum color 'blue))

(define color-set (enum-type->enum-set color))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham"))
) ;define

(define pizza (make-enum-type pizza-descriptions))

(define pizza-chicago (enum-name->enum pizza 'chicago))

;; enum-type->enum-set
;; 创建包含 enum-type 全部成员的 enum-set。
;;
;; 语法
;; ----
;; (enum-type->enum-set enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; enum-set
;; 包含该类型所有 enum 的集合。
;;
;; 注意
;; ----
;; 常用于构造全集。
;;
;; 示例
;; ----
;; (enum-set-contains? color-set color-blue) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-contains? color-set color-blue) => #t)
(check (enum-set-contains? (enum-type->enum-set pizza) pizza-chicago) => #t)

(check-report)
