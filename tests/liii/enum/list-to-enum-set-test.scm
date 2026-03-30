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

;; list->enum-set
;; 从 enum 列表创建 enum-set。
;;
;; 语法
;; ----
;; (list->enum-set enum-type list)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; list : list?
;; 由 enum 组成的列表。
;;
;; 返回值
;; ----
;; enum-set
;; 由列表成员组成的新集合。
;;
;; 注意
;; ----
;; 输入列表中的 enum 需要属于同一 enum-type。
;;
;; 示例
;; ----
;; (enum-set-contains? (list->enum-set pizza (list pizza-chicago pizza-bianca)) pizza-chicago) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-contains? (list->enum-set pizza (list pizza-chicago pizza-bianca)) pizza-chicago) => #t)

(check-report)
