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


(define color-green
  (enum-name->enum color 'green)
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


;; enum-set-adjoin!
;; 线性更新地向 enum-set 中加入成员。
;;
;; 语法
;; ----
;; (enum-set-adjoin! enum-set enum ...)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 待更新的集合。
;;
;; enum : enum?
;; 要加入的成员。
;;
;; 返回值
;; ----
;; enum-set
;; 更新后的集合。
;;
;; 注意
;; ----
;; 通常与 `enum-set-copy` 一起使用以避免修改共享数据。
;;
;; 示例
;; ----
;; (enum-set-contains? (enum-set-adjoin! (enum-set-copy reddish) color-green) color-green) => #t
;;
;; 错误处理
;; ----
;; 无。


(let ((reddish+green (enum-set-adjoin! (enum-set-copy reddish)
                       color-green
                     ) ;enum-set-adjoin!
      ) ;reddish+green
     ) ;
  (check (enum-set<? reddish reddish+green)
    =>
    #t
  ) ;check
  (check (enum-set-contains? reddish+green
           color-green
         ) ;enum-set-contains?
    =>
    #t
  ) ;check
) ;let


(check-report)
