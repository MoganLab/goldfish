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


;; enum-type
;; 获取 enum 所属的 enum-type。
;;
;; 语法
;; ----
;; (enum-type enum)
;;
;; 参数
;; ----
;; enum : enum?
;; 目标 enum。
;;
;; 返回值
;; ----
;; enum-type
;; enum 对应的枚举类型。
;;
;; 注意
;; ----
;; 返回值应与创建该 enum 的类型对象相同。
;;
;; 示例
;; ----
;; (eqv? color (enum-type color-red)) => #t
;;
;; 错误处理
;; ----
;; 无。


(check (eqv? color
         (enum-type (enum-name->enum color 'red))
       ) ;eqv?
  =>
  #t
) ;check


(check-report)
