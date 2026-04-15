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


;; enum-name
;; 获取 enum 的名称。
;;
;; 语法
;; ----
;; (enum-name enum)
;;
;; 参数
;; ----
;; enum : enum?
;; 目标 enum。
;;
;; 返回值
;; ----
;; symbol
;; enum 对应的名称。
;;
;; 注意
;; ----
;; 名称与创建 enum-type 时提供的符号一致。
;;
;; 示例
;; ----
;; (enum-name color-red) => 'red
;;
;; 错误处理
;; ----
;; 无。


(check (enum-name (enum-name->enum color 'red))
  =>
  'red
) ;check
(check (enum-name (enum-ordinal->enum color 0))
  =>
  'red
) ;check


(check-report)
