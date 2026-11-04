;; atom?
;; 判断一个值是否为 atom 记录。
;;
;; 语法
;; ----
;; (atom? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的 Scheme 值。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 value 是 atom 记录；否则返回 #f。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; 测试 make-atom 返回的对象是 atom
(check (atom? (make-atom)) => #t)
(check (atom? (make-atom :value "x")) => #t)

;; 测试非 atom 对象返回 #f
(check (atom? "string") => #f)
(check (atom? 123) => #f)
(check (atom? '()) => #f)
(check (atom? (vector)) => #f)
(check (atom? (make-env)) => #f)

(check-report)
