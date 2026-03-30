(import (liii check)
        (liii enum))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define (constantly obj)
  (lambda _ obj))

(define always (constantly #t))

(define never (constantly #f))

;; enum-set-remove
;; 返回移除满足谓词成员后的新集合。
;;
;; 语法
;; ----
;; (enum-set-remove pred enum-set)
;;
;; 参数
;; ----
;; pred : procedure?
;; 移除谓词。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; enum-set
;; 移除成员后的新集合。
;;
;; 注意
;; ----
;; 与 `enum-set-filter` 行为互补。
;;
;; 示例
;; ----
;; (enum-set-empty? (enum-set-remove always color-set)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set=? (enum-set-remove never color-set) color-set) => #t)
(check (enum-set-empty? (enum-set-remove always color-set)) => #t)

(check-report)
