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

;; enum-set-adjoin!
;; 线性更新地向 enum-set 中加入成员。
;;
;; 语法
;; ----
;; (enum-set-adjoin! enum-set enum ...)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 待更新的集合。
;;
;; enum : enum?
;; 要加入的成员。
;;
;; 返回值
;; ----
;; enum-set
;; 更新后的集合。
;;
;; 注意
;; ----
;; 通常与 `enum-set-copy` 一起使用以避免修改共享数据。
;;
;; 示例
;; ----
;; (enum-set-contains? (enum-set-adjoin! (enum-set-copy reddish) color-green) color-green) => #t
;;
;; 错误处理
;; ----
;; 无。

(let ((reddish+green (enum-set-adjoin! (enum-set-copy reddish) color-green)))
  (check (enum-set<? reddish reddish+green) => #t)
  (check (enum-set-contains? reddish+green color-green) => #t)
)

(check-report)
