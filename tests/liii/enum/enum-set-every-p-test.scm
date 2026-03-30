(import (liii check)
        (liii enum))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define empty-colors (enum-empty-set color))

(define (constantly obj)
  (lambda _ obj))

(define never (constantly #f))

;; enum-set-every?
;; 判断 enum-set 中是否所有成员都满足谓词。
;;
;; 语法
;; ----
;; (enum-set-every? pred enum-set)
;;
;; 参数
;; ----
;; pred : procedure?
;; 判定每个 enum 的谓词。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; boolean
;; 全部满足时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 空集合上返回 #t。
;;
;; 示例
;; ----
;; (enum-set-every? never empty-colors) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set-every? (lambda (e) (< (enum-ordinal e) 10)) color-set) => #t)
(check (enum-set-every? never empty-colors) => #t)

(check-report)
