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

;; make-enum-comparator
;; 创建用于比较 enum 的 SRFI-128 比较器。
;;
;; 语法
;; ----
;; (make-enum-comparator enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; comparator
;; 用于同一 enum-type 的比较器对象。
;;
;; 注意
;; ----
;; 比较器支持顺序比较和哈希。
;;
;; 示例
;; ----
;; (comparator? (make-enum-comparator pizza)) => #t
;;
;; 错误处理
;; ----
;; 无。

(define pizza-comparator (make-enum-comparator pizza))
(check (comparator? pizza-comparator) => #t)
(check (comparator-ordered? pizza-comparator) => #t)
(check (comparator-hashable? pizza-comparator) => #t)
(check (=? pizza-comparator pizza-chicago (enum-name->enum pizza 'chicago)) => #t)
(check (=? pizza-comparator pizza-bianca pizza-chicago) => #f)
(check (<? pizza-comparator pizza-bianca pizza-chicago) => #t)
(check (<? pizza-comparator pizza-bianca pizza-bianca) => #f)
(check (>? pizza-comparator pizza-chicago pizza-bianca) => #t)
(check (<=? pizza-comparator pizza-bianca pizza-chicago) => #t)
(check (>=? pizza-comparator pizza-chicago pizza-bianca) => #t)

(check-report)
