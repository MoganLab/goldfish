(import (liii check) (liii enum))


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


;; enum-set-copy
;; 复制一个 enum-set。
;;
;; 语法
;; ----
;; (enum-set-copy enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 待复制的集合。
;;
;; 返回值
;; ----
;; enum-set
;; 内容相同但对象不同的新集合。
;;
;; 注意
;; ----
;; 可用于线性更新前的防御性复制。
;;
;; 示例
;; ----
;; (enum-set=? color-set (enum-set-copy color-set)) => #t
;;
;; 错误处理
;; ----
;; 无。


(check (eqv? color-set
         (enum-set-copy color-set)
       ) ;eqv?
  =>
  #f
) ;check
(check (enum-set=? color-set
         (enum-set-copy color-set)
       ) ;enum-set=?
  =>
  #t
) ;check


(check-report)
