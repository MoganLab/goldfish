(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

;; enum-ordinal->enum
;; 通过序数获取 enum。
;;
;; 语法
;; ----
;; (enum-ordinal->enum enum-type exact-integer)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; exact-integer : integer?
;; 目标序数。
;;
;; 返回值
;; ----
;; enum or #f
;; 序数有效时返回 enum，否则返回 #f。
;;
;; 注意
;; ----
;; 越界时返回 #f。
;;
;; 示例
;; ----
;; (enum-ordinal->enum color 3) => <enum>
;;
;; 错误处理
;; ----
;; 无。

(check (enum-name (enum-ordinal->enum color 3)) => 'yellow)
(check (enum-ordinal->enum color 10) => #f)

(check-report)
