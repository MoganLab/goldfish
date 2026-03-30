(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-set (enum-type->enum-set color))

;; enum-set?
;; 判断对象是否为 enum-set。
;;
;; 语法
;; ----
;; (enum-set? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 要检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 为 enum-set 时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; enum-type 不会被识别为 enum-set。
;;
;; 示例
;; ----
;; (enum-set? color-set) => #t
;;
;; 错误处理
;; ----
;; 无。

(check (enum-set? color-set) => #t)
(check (enum-set? color) => #f)

(check-report)
