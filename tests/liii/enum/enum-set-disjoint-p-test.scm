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

(define reddish-complement
  (list->enum-set color
                  (map
                    (lambda (name)
                      (enum-name->enum color name)
                    ) ;lambda
                    (drop color-names 3)
                  ) ;map
  ) ;list->enum-set
) ;define

(define empty-colors (enum-empty-set color))

;; enum-set-disjoint?
;; 判断两个 enum-set 是否不相交。
;;
;; 语法
;; ----
;; (enum-set-disjoint? enum-set1 enum-set2)
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
;; 没有共同成员时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 空集合与任何集合都不相交。
;;
;; 示例
;; ----
;; (enum-set-disjoint? color-set empty-colors) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-disjoint? color-set empty-colors) => #t)
(check (enum-set-disjoint? color-set reddish) => #f)
(check (enum-set-disjoint? reddish reddish-complement) => #t)

(check-report)
