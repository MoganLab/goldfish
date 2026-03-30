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

;; enum-empty-set
;; 测试 `enum-empty-set` 的基本行为。
;;
;; 语法
;; ----
;; (enum-empty-set ...)
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

;; enum-empty-set
;; 创建一个空的 enum-set。
;;
;; 语法
;; ----
;; (enum-empty-set enum-type)
;;
;; 返回值
;; ------
;; enum-set

(check (enum-set-empty? (enum-empty-set pizza)) => #t)
(check (enum-set-empty? empty-colors) => #t)
(check (enum-set-empty? color-set) => #f)

(check-report)
