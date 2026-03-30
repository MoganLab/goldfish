(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

;; enum-set-empty?
;; 判断 enum-set 是否为空。
;;
;; 语法
;; ----
;; (enum-set-empty? enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; boolean
;; 为空时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 删除所有元素后应变为空集合。
;;
;; 示例
;; ----
;; (enum-set-empty? color-set) => #f
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-empty? color-set) => #f)
(check (enum-set-empty? (enum-set-delete-all! (enum-set-copy color-set) (enum-type-enums color))) => #t)

(check-report)
