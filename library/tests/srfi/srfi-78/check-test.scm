;; check
;; 对表达式求值结果进行断言。
;;
;; 语法
;; ----
;; (check expr => expected)
;; (check expr (=> equal) expected)
;;
;; 参数
;; ----
;; expr : any
;; 待求值与验证的表达式。
;; expected : any
;; 期望的求值结果。
;; equal : procedure? (可选，默认 equal?)
;; 结果比较判定过程。
;;
;; 返回值
;; ----
;; unspecified
;; 断言结果会被计入当前模式下的统计信息中。

(import (srfi srfi-78))

(check-set-mode! 'report-failed)

(check (+ 1 2) => 3)
(check '(a b c) => '(a b c))
(check (list 1 2 3) => '(1 2 3))

;; 自定义比较器
(check 42 (=> =) 42)
(check '(1 2) (=> equal?) '(1 2))

(check-report)
