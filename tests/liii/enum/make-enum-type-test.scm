(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


;; make-enum-type
;; 创建一个新的 enum-type。
;;
;; 语法
;; ----
;; (make-enum-type list)
;;
;; 参数
;; ----
;; list : list?
;; 元素为符号或 `(符号 值)` 对的列表。
;;
;; 返回值
;; ----
;; enum-type
;; 新创建的枚举类型。
;;
;; 注意
;; ----
;; 未显式指定值时，enum-value 默认等于序数。
;;
;; 示例
;; ----
;; (enum-type? (make-enum-type '(a b c))) => #t
;;
;; 错误处理
;; ----
;; 无。


(check (enum-type? (make-enum-type '(a b c)))
  =>
  #t
) ;check
(check (enum-type? (make-enum-type '((a 1) (b 2)))
       ) ;enum-type?
  =>
  #t
) ;check


(check-report)
