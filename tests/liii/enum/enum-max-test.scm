(import (liii check)
        (liii enum)
        (srfi srfi-1)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))
(define color-tangerine (enum-name->enum color 'tangerine))
(define color-blue (enum-name->enum color 'blue))
(define color-green (enum-name->enum color 'green))
(define color-set (enum-type->enum-set color))

(define reddish (list->enum-set
                  color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (take color-names 3))
                  ) ;map
) ;define

(define ~reddish (list->enum-set
                   color
                   (map (lambda (ord)
                          (enum-name->enum color ord))
                        (drop color-names 3))
                   ) ;map
) ;define

(define empty-colors (enum-empty-set color))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi     "mushrooms")
    (bianca     "ricotta and mozzarella")
    (chicago    "deep-dish")
    (hawaiian   "pineapple and ham"))
) ;define

(define pizza-names (map car pizza-descriptions))
(define pizza (make-enum-type pizza-descriptions))
(define pizza-chicago (enum-name->enum pizza 'chicago))
(define pizza-bianca (enum-name->enum pizza 'bianca))

(define (constantly obj)
  (lambda _ obj)
) ;define

(define always (constantly #t))
(define never (constantly #f))

(define (fresh-sets proc eset1 eset2)
  (proc (enum-set-copy eset1) (enum-set-copy eset2))
) ;define

;; enum-max
;; 测试 `enum-max` 的基本行为。
;;
;; 语法
;; ----
;; (enum-max ...)
;;
;; 参数
;; ----
;; 见原有测试断言。
;;
;; 返回值
;; ----
;; 见断言结果。
;;
;; 注意
;; ----
;; 本文件仅负责结构迁移和回归验证。
;;
;; 示例
;; ----
;; 见下方 check 断言。
;;
;; 错误处理
;; ----
;; 由原始测试覆盖。

;; enum-max
;; 获取 enum-type 中最大的 enum。
;;
;; 语法
;; ----
;; (enum-max enum-type)
;;
;; 返回值
;; ------
;; enum 对象

(check (enum-name (enum-max color)) => 'violet)
(check (enum-name (enum-max pizza)) => 'hawaiian)

(check-report)
