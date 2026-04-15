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


;; enum-ordinal
;; 获取 enum 的序数。
;;
;; 语法
;; ----
;; (enum-ordinal enum)
;;
;; 参数
;; ----
;; enum : enum?
;; 目标 enum。
;;
;; 返回值
;; ----
;; exact-integer
;; 从 0 开始的序数。
;;
;; 注意
;; ----
;; 序数按照 enum-type 中的定义顺序分配。
;;
;; 示例
;; ----
;; (enum-ordinal color-red) => 0
;;
;; 错误处理
;; ----
;; 无。


(check (enum-ordinal (enum-name->enum color 'red)
       ) ;enum-ordinal
  =>
  0
) ;check
(check (enum-ordinal (enum-name->enum color 'blue)
       ) ;enum-ordinal
  =>
  6
) ;check
(check (enum-ordinal (enum-ordinal->enum color 0)
       ) ;enum-ordinal
  =>
  0
) ;check


(check-report)
