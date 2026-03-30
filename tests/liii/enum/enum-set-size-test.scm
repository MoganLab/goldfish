(import (liii check)
        (liii enum))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define empty-colors (enum-empty-set color))

;; enum-set-size
;; 获取 enum-set 中成员数量。
;;
;; 语法
;; ----
;; (enum-set-size enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; exact-integer
;; 集合中的成员数量。
;;
;; 注意
;; ----
;; 空集合大小为 0。
;;
;; 示例
;; ----
;; (enum-set-size empty-colors) => 0
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-size color-set) => (length color-names))
(check (enum-set-size empty-colors) => 0)

(check-report)
