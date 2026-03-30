(import (liii check)
        (liii enum)
        (srfi srfi-1))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define reddish
  (list->enum-set color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (take color-names 3))))

;; enum-set-projection
;; 将 enum-set 投影到另一个 enum-type。
;;
;; 语法
;; ----
;; (enum-set-projection enum-type-or-set enum-set)
;;
;; 参数
;; ----
;; enum-type-or-set : enum-type? or enum-set?
;; 目标类型或集合。
;;
;; enum-set : enum-set?
;; 源集合。
;;
;; 返回值
;; ----
;; enum-set
;; 投影后的新集合。
;;
;; 注意
;; ----
;; 投影到同一类型时应保持原集合内容。
;;
;; 示例
;; ----
;; (enum-set=? (enum-set-projection color reddish) reddish) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set=? (enum-set-projection color reddish) reddish) => #t)

(check-report)
