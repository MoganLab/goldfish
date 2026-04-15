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


(define (fresh-sets proc eset1 eset2)
  (proc (enum-set-copy eset1)
    (enum-set-copy eset2)
  ) ;proc
) ;define


;; enum-set-xor!
;; 线性更新地计算对称差集。
;;
;; 语法
;; ----
;; (enum-set-xor! enum-set1 enum-set2)
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
;; 测试中通过 fresh-sets 避免修改共享测试数据。
;;
;; 示例
;; ----
;; (enum-set=? color-set (fresh-sets enum-set-xor! reddish reddish-complement)) => #t
;;
;; 错误处理
;; ----
;; 无。


(check (enum-set=? color-set
         (fresh-sets enum-set-xor!
           reddish
           reddish-complement
         ) ;fresh-sets
       ) ;enum-set=?
  =>
  #t
) ;check


(check-report)
