(import (liii check)
        (liii enum)
        (srfi srfi-1)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-green (enum-name->enum color 'green))

(define reddish
  (list->enum-set color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (take color-names 3)
                  ) ;map
  ) ;list->enum-set
) ;define

;; enum-set-adjoin
;; 返回包含新增成员的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-adjoin enum-set enum ...)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 原集合。
;;
;; enum : enum?
;; 要加入的成员。
;;
;; 返回值
;; ----
;; enum-set
;; 添加成员后的新集合。
;;
;; 注意
;; ----
;; 不会修改原集合。
;;
;; 示例
;; ----
;; (enum-set-contains? (enum-set-adjoin reddish color-green) color-green) => #t
;;
;; 错误处理
;; ----
;; 无。

(let ((reddish+green (enum-set-adjoin reddish color-green)))
  (check (enum-set<? reddish reddish+green) => #t)
  (check (enum-set-contains? reddish+green color-green) => #t)
) ;let

(check-report)
