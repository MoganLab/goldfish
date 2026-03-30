(import (liii check)
        (liii enum)
        (srfi srfi-1))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define reddish
  (list->enum-set color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (take color-names 3))))

(define reddish-complement
  (list->enum-set color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (drop color-names 3))))

;; enum-set-complement
;; 返回集合相对于全集的补集。
;;
;; 语法
;; ----
;; (enum-set-complement enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; enum-set
;; 补集结果。
;;
;; 注意
;; ----
;; 全集的补集为空集合。
;;
;; 示例
;; ----
;; (enum-set=? (enum-set-complement reddish) reddish-complement) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-empty? (enum-set-complement color-set)) => #t)
(check (enum-set=? (enum-set-complement reddish) reddish-complement) => #t)

(check-report)
