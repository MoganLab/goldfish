(import (liii check)
        (liii enum)
        (srfi srfi-1)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-blue (enum-name->enum color 'blue))

(define color-set (enum-type->enum-set color))

(define reddish
  (list->enum-set color
                  (map
                    (lambda (name)
                      (enum-name->enum color name)
                    ) ;lambda
                    (take color-names 3)
                  ) ;map
  ) ;list->enum-set
) ;define

;; enum-set-count
;; 统计满足谓词的成员数量。
;;
;; 语法
;; ----
;; (enum-set-count pred enum-set)
;;
;; 参数
;; ----
;; pred : procedure?
;; 判断成员的谓词。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; exact-integer
;; 满足谓词的成员数量。
;;
;; 注意
;; ----
;; 可用于快速做基数统计。
;;
;; 示例
;; ----
;; (enum-set-count (lambda (e) (enum=? e color-blue)) color-set) => 1
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-count (lambda (e) (enum=? e color-blue)) color-set) => 1)
(check (enum-set-count (lambda (e) (enum=? e color-blue)) reddish) => 0)

(check-report)
