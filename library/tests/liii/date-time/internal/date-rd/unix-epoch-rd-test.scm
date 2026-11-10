;; unix-epoch-rd
;; UNIX 纪元（1970-01-01）对应的 raté die 天数。
;;
;; 语法
;; ----
;; unix-epoch-rd
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; integer? 1970-01-01 的 raté die 值。

(import (liii check) (liii date-time date) (liii date-time internal date-rd))

(check-set-mode! 'report-failed)

(check unix-epoch-rd => 719163)
(check unix-epoch-rd => (calc-date->rata-die (make-date 1970 1 1)))

(check-report)
