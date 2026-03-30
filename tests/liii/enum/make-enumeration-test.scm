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

;; make-enumeration
;; 创建包含所有名称的 enum-set。
;;
;; 语法
;; ----
;; (make-enumeration symbol-list)
;;
;; 参数
;; ----
;; symbol-list : list?
;; 符号名称列表。
;;
;; 返回值
;; ----
;; enum-set
;; 包含所有名称对应 enum 的集合。
;;
;; 注意
;; ----
;; R6RS 兼容接口，集合中的 value 与 name 相同。
;;
;; 示例
;; ----
;; (enum-set-every? (lambda (e) (eqv? (enum-name e) (enum-value e))) (make-enumeration '(red yellow green))) => #t
;;
;; 错误处理
;; ----
;; 无。

(let* ((ds '(red yellow green))
       (us-traffic-light (make-enumeration ds)))
  (check (enum-set-every? (lambda (e) (eqv? (enum-name e) (enum-value e)))
                          us-traffic-light)
         =>
         #t))

(check-report)
