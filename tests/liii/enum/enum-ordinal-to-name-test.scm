(import (liii check)
        (liii enum)
        (srfi srfi-1)
        (srfi srfi-128))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))
(define color-tangerine (enum-name->enum color 'tangerine))
(define color-blue (enum-name->enum color 'blue))
(define color-green (enum-name->enum color 'green))
(define color-set (enum-type->enum-set color))

(define reddish
  (list->enum-set color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (take color-names 3))))

(define reddish-complement
  (list->enum-set color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (drop color-names 3))))

(define empty-colors (enum-empty-set color))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi "mushrooms")
    (bianca "ricotta and mozzarella")
    (chicago "deep-dish")
    (hawaiian "pineapple and ham")))

(define pizza-names (map car pizza-descriptions))
(define pizza (make-enum-type pizza-descriptions))
(define pizza-chicago (enum-name->enum pizza 'chicago))
(define pizza-bianca (enum-name->enum pizza 'bianca))

(define (constantly obj)
  (lambda _ obj))

(define always (constantly #t))
(define never (constantly #f))

(define (fresh-sets proc eset1 eset2)
  (proc (enum-set-copy eset1) (enum-set-copy eset2)))

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

(check (enum-ordinal->name color 0) => 'red)
(check (enum-ordinal->name pizza 3) => 'chicago)

(check-report)
