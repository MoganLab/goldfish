;; bigrat-den
;; 获取 <bigrat> 的分母。
;;
;; 语法
;; ----
;; (bigrat-den r)
;;
;; 参数
;; ----
;; r : bigrat? 有理数。
;;
;; 返回值
;; ----
;; bigint? 分母。

(import (liii check) (liii bigrat) (liii bigint))

(check-set-mode! 'report-failed)

(check (bigint->string (bigrat-den (make-bigrat 6 -4))) => "2")

(check-report)
