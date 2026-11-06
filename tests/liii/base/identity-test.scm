;; identity
;; 原样返回传入的参数。
;;
;; 语法
;; ----
;; (identity x)
;;
;; 参数
;; ----
;; x : any
;; 待返回的值。
;;
;; 返回值
;; ----
;; any
;; 返回与参数 x 完全相同的值（eq? 意义下）。
;;
;; 注意
;; ----
;; identity 常用于把常量值转换为过程的场合，例如 (map identity list)。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii base))


(check-set-mode! 'report-failed)


;; 基本值
(check (identity 1) => 1)
(check (identity "str") => "str")
(check (identity 'sym) => 'sym)
(check (identity #t) => #t)
(check (identity #f) => #f)
(check (identity '()) => '())

;; 返回的是同一个对象
(check-true (eq? (identity list) list))

;; 可用于 map 等高阶函数
(check (map identity '(1 2 3)) => '(1 2 3))
(check (map identity '("a" "b")) => '("a" "b"))


(check-report)
