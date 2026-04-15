(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


(define color-red
  (enum-name->enum color 'red)
) ;define


;; enum-type?
;; 判断对象是否为 enum-type。
;;
;; 语法
;; ----
;; (enum-type? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 要检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 当对象为 enum-type 时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 仅识别枚举类型对象本身。
;;
;; 示例
;; ----
;; (enum-type? color) => #t
;; (enum-type? color-red) => #f
;;
;; 错误处理
;; ----
;; 无。


(check (enum-type? color) => #t)
(check (enum-type? color-red) => #f)
(check (enum-type? 'not-a-type) => #f)


(check-report)
