(import (liii check)
        (liii enum)
        (srfi srfi-1)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-tangerine (enum-name->enum color 'tangerine))

(define reddish
  (list->enum-set color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (take color-names 3)
                  ) ;map
  ) ;list->enum-set
) ;define

;; enum-set-delete
;; 返回移除指定成员的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-delete enum-set enum ...)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 原集合。
;;
;; enum : enum?
;; 要移除的成员。
;;
;; 返回值
;; ----
;; enum-set
;; 删除成员后的新集合。
;;
;; 注意
;; ----
;; 不会修改原集合。
;;
;; 示例
;; ----
;; (enum-set-contains? (enum-set-delete reddish color-tangerine) color-tangerine) => #f
;;
;; 错误处理
;; ----
;; 无。

(let ((reddish* (enum-set-delete reddish color-tangerine)))
  (check (enum-set<? reddish* reddish) => #t)
  (check (enum-set-contains? reddish* color-tangerine) => #f)
) ;let

(check-report)
