(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; check
;; 对表达式结果进行精确断言。
;;
;; 语法
;; ----
;; (check expr => expected)
;;
;; 参数
;; ----
;; expr : any
;; 要求值并检查结果的表达式。
;; expected : any
;; 期望结果，使用 `equal?` 进行比较。
;;
;; 返回值
;; ----
;; 无
;; 断言结果会被纳入 `check-report` 统计。

(check (+ 1 2) => 3)
(check '(a b c) => '(a b c))
(check (list 1 2 3) => '(1 2 3))

(check-report)
