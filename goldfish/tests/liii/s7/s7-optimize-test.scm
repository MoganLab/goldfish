;; s7-optimize
;; S7 内置的代码优化与调试辅助函数。
;;
;; 语法
;; ----
;; (s7-optimize expr)
;;
;; 参数
;; ----
;; expr : any?
;; 待优化的 Scheme 表达式代码。
;;
;; 返回值
;; ----
;; any?
;; 优化执行的结果。
;;
;; 说明
;; ----
;; S7 底层代码优化器与调试辅助接口。

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (procedure? s7-optimize) => #t)

(check-report)
