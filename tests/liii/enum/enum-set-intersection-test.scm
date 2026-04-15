(import (liii check)
  (liii enum)
  (srfi srfi-1)
) ;import


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


(define reddish
  (list->enum-set color
    (map (lambda (name)
           (enum-name->enum color name)
         ) ;lambda
      (take color-names 3)
    ) ;map
  ) ;list->enum-set
) ;define


(define reddish-complement
  (list->enum-set color
    (map (lambda (name)
           (enum-name->enum color name)
         ) ;lambda
      (drop color-names 3)
    ) ;map
  ) ;list->enum-set
) ;define


;; enum-set-intersection
;; 返回两个集合的交集。
;;
;; 语法
;; ----
;; (enum-set-intersection enum-set1 enum-set2)
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
;; 两个集合的交集。
;;
;; 注意
;; ----
;; 不相交时结果为空集合。
;;
;; 示例
;; ----
;; (enum-set-empty? (enum-set-intersection reddish reddish-complement)) => #t
;;
;; 错误处理
;; ----
;; 无。


(check (enum-set-empty? (enum-set-intersection reddish
                          reddish-complement
                        ) ;enum-set-intersection
       ) ;enum-set-empty?
  =>
  #t
) ;check


(check-report)
