(import (liii check)
        (liii enum))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define color-tangerine (enum-name->enum color 'tangerine))

(define color-blue (enum-name->enum color 'blue))

;; enum-set
;; 创建包含指定 enum 的 enum-set。
;;
;; 语法
;; ----
;; (enum-set enum-type enum ...)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; enum : enum?
;; 要加入集合的枚举项。
;;
;; 返回值
;; ----
;; enum-set
;; 由给定 enum 构成的新集合。
;;
;; 注意
;; ----
;; 未列出的 enum 不会出现在结果中。
;;
;; 示例
;; ----
;; (enum-set-contains? (enum-set color color-red color-blue) color-red) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-contains? (enum-set color color-red color-blue) color-red) => #t)
(check (enum-set-contains? (enum-set color color-red color-blue) color-tangerine) => #f)

(check-report)
