;; %syntax-case-counter
;; 获取当前 syntax-case 宏系统内部生成符号的自增计数器值。
;;
;; 语法
;; ----
;; (%syntax-case-counter)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; -----
;; integer
;; 当前计数器整数值。

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 返回非负整数
(check-true (integer? (%syntax-case-counter)))
(check-true (>= (%syntax-case-counter) 0))

(check-report)
