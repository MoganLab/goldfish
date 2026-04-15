(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


;; enum-name->ordinal
;; 通过名称获取 enum 的序数。
;;
;; 语法
;; ----
;; (enum-name->ordinal enum-type symbol)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; symbol : symbol?
;; enum 名称。
;;
;; 返回值
;; ----
;; exact-integer
;; 目标 enum 的序数。
;;
;; 注意
;; ----
;; 示例覆盖有效名称的常见用法。
;;
;; 示例
;; ----
;; (enum-name->ordinal color 'red) => 0
;;
;; 错误处理
;; ----
;; 原始测试未覆盖错误分支。


(check (enum-name->ordinal color 'red)
  =>
  0
) ;check
(check (enum-name->ordinal color 'blue)
  =>
  6
) ;check


(check-report)
