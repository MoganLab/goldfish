(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define color-tangerine (enum-name->enum color 'tangerine))

(define color-blue (enum-name->enum color 'blue))

;; enum<=?
;; 判断 enum 的序数是否非递减。
;;
;; 语法
;; ----
;; (enum<=? enum1 enum2 ...)
;;
;; 参数
;; ----
;; enum1 : enum?
;; 第一个 enum。
;;
;; enum2 : enum?
;; 其余待比较的 enum。
;;
;; 返回值
;; ----
;; boolean
;; 非递减时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 允许相等元素。
;;
;; 示例
;; ----
;; (enum<=? color-tangerine color-tangerine) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum<=? color-red color-tangerine) => #t)
(check (enum<=? color-tangerine color-tangerine) => #t)
(check (enum<=? color-tangerine color-red) => #f)
(check (enum<=? color-red color-blue color-blue) => #t)
(check (enum<=? color-blue color-blue color-red) => #f)

(check-report)
