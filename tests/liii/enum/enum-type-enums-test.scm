(import (liii check)
        (liii enum)
        (srfi srfi-1))

(check-set-mode! 'report-failed)

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

;; enum-type-enums
;; 获取 enum-type 中所有 enum 的列表。
;;
;; 语法
;; ----
;; (enum-type-enums enum-type)
;;
;; 参数
;; ----
;; enum-type : enum-type?
;; 目标枚举类型。
;;
;; 返回值
;; ----
;; list
;; 按序数排序的 enum 列表。
;;
;; 注意
;; ----
;; 列表顺序与定义顺序一致。
;;
;; 示例
;; ----
;; (map enum-name (enum-type-enums color)) => color-names
;;
;; 错误处理
;; ----
;; 无。

(check (enum-type-size color) => (length (enum-type-enums color)))
(check color-names => (map enum-name (enum-type-enums color)))
(check (iota (enum-type-size color)) => (map enum-ordinal (enum-type-enums color)))

(check-report)
