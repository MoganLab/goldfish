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


;; enum-set-complement!
;; 线性更新地计算补集。
;;
;; 语法
;; ----
;; (enum-set-complement! enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 待更新的集合。
;;
;; 返回值
;; ----
;; enum-set
;; 补集结果。
;;
;; 注意
;; ----
;; 通常配合 `enum-set-copy` 使用。
;;
;; 示例
;; ----
;; (enum-set-empty? (enum-set-complement! (enum-set-copy color-set))) => #t
;;
;; 错误处理
;; ----
;; 无。


(check (enum-set-empty? (enum-set-complement! (enum-set-copy color-set)
                        ) ;enum-set-complement!
       ) ;enum-set-empty?
  =>
  #t
) ;check
(check (enum-set=? (enum-set-complement! (enum-set-copy reddish)
                   ) ;enum-set-complement!
         reddish-complement
       ) ;enum-set=?
  =>
  #t
) ;check


(check-report)
