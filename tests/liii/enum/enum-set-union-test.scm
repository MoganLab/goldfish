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

;; enum-set-union
;; 返回两个集合的并集。
;;
;; 语法
;; ----
;; (enum-set-union enum-set1 enum-set2)
;;
;; 参数
;; ----
;; enum-set1 : enum-set?
;; 第一个集合。
;;
;; enum-set2 : enum-set?
;; 第二个集合。
;;
;; 返回值
;; ----
;; enum-set
;; 两个集合的并集。
;;
;; 注意
;; ----
;; 不会修改输入集合。
;;
;; 示例
;; ----
;; (enum-set=? color-set (enum-set-union reddish reddish-complement)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set=? color-set (enum-set-union reddish reddish-complement)) => #t)

(check-report)
