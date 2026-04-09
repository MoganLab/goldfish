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

;; enum-set-xor
;; 返回两个集合的对称差集。
;;
;; 语法
;; ----
;; (enum-set-xor enum-set1 enum-set2)
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
;; enum-set
;; 对称差集结果。
;;
;; 注意
;; ----
;; 同一集合异或自身应为空集合。
;;
;; 示例
;; ----
;; (enum-set-empty? (enum-set-xor reddish reddish)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set=? color-set (enum-set-xor reddish reddish-complement)) => #t)
(check (enum-set-empty? (enum-set-xor reddish reddish)) => #t)

(check-report)
