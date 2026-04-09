(import (liii check)
        (liii enum)
        (srfi srfi-1)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define reddish
  (list->enum-set color
                  (map
                    (lambda (name)
                      (enum-name->enum color name)
                    ) ;lambda
                    (take color-names 3)
                  ) ;map
  ) ;list->enum-set
) ;define

;; enum-set<=?
;; 判断第一个集合是否为第二个集合的子集。
;;
;; 语法
;; ----
;; (enum-set<=? enum-set1 enum-set2)
;;
;; 参数
;; ----
;; enum-set1 : enum-set?
;; 第一个集合。
;;
;; enum-set2 : enum-set?
;; 第二个集合。
;;
;; 返回值
;; ----
;; boolean
;; 子集时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 相同集合视为子集。
;;
;; 示例
;; ----
;; (enum-set<=? color-set color-set) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set<=? reddish color-set) => #t)
(check (enum-set<=? color-set reddish) => #f)
(check (enum-set<=? color-set color-set) => #t)

(check-report)
