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


(define color-set
  (enum-type->enum-set color)
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


;; enum-set-difference
;; 返回第一个集合相对第二个集合的差集。
;;
;; 语法
;; ----
;; (enum-set-difference enum-set1 enum-set2)
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
;; 差集结果。
;;
;; 注意
;; ----
;; 不会修改输入集合。
;;
;; 示例
;; ----
;; (enum-set=? reddish-complement (enum-set-difference color-set reddish)) => #t
;;
;; 错误处理
;; ----
;; 无。


(check (enum-set=? reddish-complement
         (enum-set-difference color-set reddish)
       ) ;enum-set=?
  =>
  #t
) ;check


(check-report)
