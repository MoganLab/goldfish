(import (liii check)
        (liii enum))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define color-set (enum-type->enum-set color))

(define (constantly obj)
  (lambda _ obj))

(define always (constantly #t))

(define never (constantly #f))

;; enum-set-filter
;; 返回满足谓词的成员构成的新集合。
;;
;; 语法
;; ----
;; (enum-set-filter pred enum-set)
;;
;; 参数
;; ----
;; pred : procedure?
;; 过滤谓词。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; enum-set
;; 过滤后的新集合。
;;
;; 注意
;; ----
;; 不会修改原集合。
;;
;; 示例
;; ----
;; (enum-set-empty? (enum-set-filter never color-set)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set<? (enum-set-filter (lambda (e) (enum=? e color-red)) color-set) color-set) => #t)
(check (enum-set=? (enum-set-filter always color-set) color-set) => #t)
(check (enum-set-empty? (enum-set-filter never color-set)) => #t)

(check-report)
