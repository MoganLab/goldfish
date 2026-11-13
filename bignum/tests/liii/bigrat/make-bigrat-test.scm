;; make-bigrat
;; 构造并约分规范化的 <bigrat> 有理数。
;;
;; 语法
;; ----
;; (make-bigrat num den)
;;
;; 参数
;; ----
;; num : integer? | bigint? 分子。
;; den : integer? | bigint? 分母（非零）。
;;
;; 返回值
;; ----
;; bigrat? 规范化后的 <bigrat> 实例。

(import (liii check) (liii bigrat) (liii bigint))

(check-set-mode! 'report-failed)

(check (bigrat->string (make-bigrat 6 -4)) => "-3/2")
(check-catch 'value-error (make-bigrat 1 0))

(check-report)
