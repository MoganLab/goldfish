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


;; enum-set-indexer
;; 返回将名称映射到序数的过程。
;;
;; 语法
;; ----
;; (enum-set-indexer enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; procedure?
;; 接受名称并返回序数或 #f 的过程。
;;
;; 注意
;; ----
;; 不在集合所属类型中的名称返回 #f。
;;
;; 示例
;; ----
;; ((enum-set-indexer reddish) 'red) => 0
;;
;; 错误处理
;; ----
;; 无。


(let ((idx (enum-set-indexer reddish)))
  (check (idx 'red) => 0)
  (check (idx 'green) => 4)
  (check (idx 'margherita) => #f)
) ;let


(check-report)
