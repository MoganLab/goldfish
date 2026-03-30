(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define empty-colors (enum-empty-set color))

;; enum-set=?
;; 判断两个 enum-set 是否相等。
;;
;; 语法
;; ----
;; (enum-set=? enum-set1 enum-set2)
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
;; boolean
;; 成员完全相同时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 复制出的集合应与原集合相等。
;;
;; 示例
;; ----
;; (enum-set=? color-set (enum-set-copy color-set)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set=? color-set (enum-set-copy color-set)) => #t)
(check (enum-set=? color-set empty-colors) => #f)

(check-report)
