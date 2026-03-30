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

;; enum-set-delete-all!
;; 线性更新地移除列表中的所有成员。
;;
;; 语法
;; ----
;; (enum-set-delete-all! enum-set list)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 待更新的集合。
;;
;; list : list?
;; 要移除的 enum 列表。
;;
;; 返回值
;; ----
;; enum-set
;; 更新后的集合。
;;
;; 注意
;; ----
;; 适合与 `enum-set-copy` 配合使用。
;;
;; 示例
;; ----
;; (enum-set<? (enum-set-delete-all! (enum-set-copy reddish) (list color-tangerine)) reddish) => #t
;;
;; 错误处理
;; ----
;; 无。

(let ((reddish** (enum-set-delete-all! (enum-set-copy reddish) (list color-tangerine))))
  (check (enum-set<? reddish** reddish) => #t)
) ;let

(check-report)
