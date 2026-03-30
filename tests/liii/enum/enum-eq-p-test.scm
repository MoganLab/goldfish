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

;; enum=?
;; 判断多个 enum 是否相等。
;;
;; 语法
;; ----
;; (enum=? enum1 enum2 ...)
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
;; 所有 enum 相同则返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 相等基于同一枚举对象。
;;
;; 示例
;; ----
;; (enum=? color-red (enum-ordinal->enum color 0)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum=? color-red (enum-ordinal->enum color 0)) => #t)
(check (enum=? color-red color-tangerine) => #f)
(check (enum=? color-red color-red color-red) => #t)
(check (enum=? color-red color-red color-tangerine) => #f)

(check-report)
