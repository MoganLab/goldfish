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

(check (enum-value (enum-name->enum pizza 'funghi)) => "mushrooms")
(check (enum-value (enum-name->enum color 'blue)) => 6)

(check-report)
