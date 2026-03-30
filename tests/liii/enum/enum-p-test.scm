(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

;; enum?
;; 判断对象是否为 enum。
;;
;; 语法
;; ----
;; (enum? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 要检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 当对象为 enum 时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; enum-type 本身不是 enum。
;;
;; 示例
;; ----
;; (enum? color-red) => #t
;; (enum? color) => #f
;;
;; 错误处理
;; ----
;; 无。

(check (enum? color-red) => #t)
(check (enum? color) => #f)
(check (enum? 'z) => #f)

(check-report)
