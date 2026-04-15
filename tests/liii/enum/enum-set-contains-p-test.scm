(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


(define color-blue
  (enum-name->enum color 'blue)
) ;define


(define color-set
  (enum-type->enum-set color)
) ;define


;; enum-set-contains?
;; 判断 enum 是否属于 enum-set。
;;
;; 语法
;; ----
;; (enum-set-contains? enum-set enum)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; enum : enum?
;; 待检查的成员。
;;
;; 返回值
;; ----
;; boolean
;; 在集合中时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 示例通过删除成员后的结果验证行为。
;;
;; 示例
;; ----
;; (enum-set-contains? color-set color-blue) => #t
;;
;; 错误处理
;; ----
;; 无。


(check (enum-set-contains? color-set
         color-blue
       ) ;enum-set-contains?
  =>
  #t
) ;check
(check (enum-set-contains? (enum-set-delete! (enum-set-copy color-set)
                             color-blue
                           ) ;enum-set-delete!
         color-blue
       ) ;enum-set-contains?
  =>
  #f
) ;check


(check-report)
