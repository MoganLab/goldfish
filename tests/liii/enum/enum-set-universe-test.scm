(import (liii check)
        (liii enum)
        (srfi srfi-1))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

(define reddish
  (list->enum-set color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (take color-names 3))))

;; enum-set-universe
;; 获取包含 enum-type 全部成员的全集。
;;
;; 语法
;; ----
;; (enum-set-universe enum-set)
;;
;; 参数
;; ----
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; enum-set
;; 对应 enum-type 的全集。
;;
;; 注意
;; ----
;; 结果与 `enum-type->enum-set` 的全集一致。
;;
;; 示例
;; ----
;; (enum-set=? color-set (enum-set-universe reddish)) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set=? color-set (enum-set-universe reddish)) => #t)

(check-report)
