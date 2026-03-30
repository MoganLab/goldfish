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

(define color-green (enum-name->enum color 'green))

;; enum>?
;; 判断 enum 的序数是否严格递减。
;;
;; 语法
;; ----
;; (enum>? enum1 enum2 ...)
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
;; 严格递减时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 比较基于序数。
;;
;; 示例
;; ----
;; (enum>? color-blue color-green color-red) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum>? color-blue color-green color-red) => #t)
(check (enum>? color-blue color-red color-red) => #f)
(check (enum>? color-red color-tangerine) => #f)

(check-report)
