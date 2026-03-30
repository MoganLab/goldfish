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

;; enum-set-copy
;; 复制一个 enum-set。
;;
;; 语法
;; ----
;; (enum-set-copy enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 待复制的集合。
;;
;; 返回值
;; ----
;; enum-set
;; 内容相同但对象不同的新集合。
;;
;; 注意
;; ----
;; 可用于线性更新前的防御性复制。
;;
;; 示例
;; ----
;; (enum-set=? color-set (enum-set-copy color-set)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (eqv? color-set (enum-set-copy color-set)) => #f)
(check (enum-set=? color-set (enum-set-copy color-set)) => #t)

(check-report)
