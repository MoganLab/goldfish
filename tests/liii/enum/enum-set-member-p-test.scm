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

;; enum-set-member?
;; 判断符号是否是 enum-set 的成员名称。
;;
;; 语法
;; ----
;; (enum-set-member? symbol enum-set)
;;
;; 参数
;; ----
;; symbol : symbol?
;; 待检查的名称。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; boolean
;; 名称在集合中时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 基于名称而不是 enum 对象本身判断。
;;
;; 示例
;; ----
;; (enum-set-member? 'red reddish) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-member? 'red reddish) => #t)
(check (enum-set-member? 'blue reddish) => #f)

(check-report)
