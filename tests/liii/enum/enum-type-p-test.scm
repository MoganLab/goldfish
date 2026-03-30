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

;; enum-type?
;; 判断对象是否为 enum-type。
;;
;; 语法
;; ----
;; (enum-type? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 要检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 当对象为 enum-type 时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 仅识别枚举类型对象本身。
;;
;; 示例
;; ----
;; (enum-type? color) => #t
;; (enum-type? color-red) => #f
;;
;; 错误处理
;; ----
;; 无。

(check (enum-type? color) => #t)
(check (enum-type? color-red) => #f)
(check (enum-type? 'not-a-type) => #f)

(check-report)
