;; bigrat-num
;; 获取 <bigrat> 的分子。
;;
;; 语法
;; ----
;; (bigrat-num r)
;;
;; 参数
;; ----
;; r : bigrat? 有理数。
;;
;; 返回值
;; ----
;; bigint? 分子。

(import (liii check) (liii bigrat) (liii bigint))

(check-set-mode! 'report-failed)

(check (bigint->string (bigrat-num (make-bigrat -6 4))) => "-3")

(check-report)
