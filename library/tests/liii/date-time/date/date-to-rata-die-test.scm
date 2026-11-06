(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date->rata-die
;; 将日期转换为 Rata Die 日序数（公历 0001-01-01 为 Day 1）。
;;
;; 语法
;; ----
;; (date->rata-die date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? Rata Die 日序数。
;;
;; 示例
;; ----
;; (date->rata-die (make-date 1 1 1)) => 1
;; (date->rata-die (make-date 2025 10 15)) => 739539
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date->rata-die (make-date 1 1 1)) => 1)
(check (date->rata-die (make-date 2025 10 15)) => 739539)
(check (date->rata-die (make-date 0 12 30)) => -1)
(check (date->rata-die (make-date 0 1 1)) => -365)
(check (date->rata-die (make-date -1 12 31)) => -366)

(check-catch 'type-error (date->rata-die "2025-10-15"))
(check-catch 'type-error (date->rata-die 1))

(check-report)
