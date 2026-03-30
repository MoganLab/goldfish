(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

;; enum-name->enum
;; 通过名称获取 enum。
;;
;; 语法
;; ----
;; (enum-name->enum enum-type symbol)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; symbol : symbol?
;; enum 名称。
;;
;; 返回值
;; ----
;; enum or #f
;; 找到时返回 enum，未找到时返回 #f。
;;
;; 注意
;; ----
;; 不会抛错，未命中直接返回 #f。
;;
;; 示例
;; ----
;; (enum-name->enum color 'green) => <enum>
;; (enum-name->enum color 'mushroom) => #f
;;
;; 错误处理
;; ----
;; 无。

(check (enum-name (enum-name->enum color 'green)) => 'green)
(check (enum-name->enum color 'mushroom) => #f)

(check-report)
