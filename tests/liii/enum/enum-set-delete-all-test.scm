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


(define color-tangerine
  (enum-name->enum color 'tangerine)
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


;; enum-set-delete-all
;; 返回移除列表中所有成员的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-delete-all enum-set list)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 原集合。
;;
;; list : list?
;; 要移除的 enum 列表。
;;
;; 返回值
;; ----
;; enum-set
;; 删除成员后的新集合。
;;
;; 注意
;; ----
;; 列表中的成员会被批量删除。
;;
;; 示例
;; ----
;; (enum-set<? (enum-set-delete-all reddish (list color-tangerine)) reddish) => #t
;;
;; 错误处理
;; ----
;; 无。


(let ((reddish* (enum-set-delete-all reddish
                  (list color-tangerine)
                ) ;enum-set-delete-all
      ) ;reddish*
     ) ;
  (check (enum-set<? reddish* reddish)
    =>
    #t
  ) ;check
) ;let


(check-report)
