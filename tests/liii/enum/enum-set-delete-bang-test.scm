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

;; enum-set-delete!
;; 线性更新地移除指定成员。
;;
;; 语法
;; ----
;; (enum-set-delete! enum-set enum ...)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 待更新的集合。
;;
;; enum : enum?
;; 要移除的成员。
;;
;; 返回值
;; ----
;; enum-set
;; 更新后的集合。
;;
;; 注意
;; ----
;; 通常先复制再调用以保持测试独立。
;;
;; 示例
;; ----
;; (enum-set-contains? (enum-set-delete! (enum-set-copy reddish) color-tangerine) color-tangerine) => #f
;;
;; 错误处理
;; ----
;; 无。

(let ((reddish* (enum-set-delete! (enum-set-copy reddish) color-tangerine)))
  (check (enum-set<? reddish* reddish) => #t)
  (check (enum-set-contains? reddish* color-tangerine) => #f)
) ;let

(check-report)
