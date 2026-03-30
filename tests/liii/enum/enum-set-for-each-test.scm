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

;; enum-set-for-each
;; 对集合中的每个成员执行过程。
;;
;; 语法
;; ----
;; (enum-set-for-each proc enum-set)
;;
;; 参数
;; ----
;; proc : procedure?
;; 要执行的过程。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。
;;
;; 注意
;; ----
;; 可通过副作用观察执行次数。
;;
;; 示例
;; ----
;; (let ((n 0)) (enum-set-for-each (lambda (_) (set! n (+ n 1))) color-set) n) => 8
;;
;; 错误处理
;; ----
;; 无。

(check (let ((n 0))
         (enum-set-for-each (lambda (_) (set! n (+ n 1))) color-set)
         n)
       =>
       (length color-names))

(check-report)
