;; bigint-signum
;; 获取 bigint 的符号。
;;
;; 语法
;; ----
;; (bigint-signum n)
;;
;; 参数
;; ----
;; n : bigint?
;;   bigint 记录。
;;
;; 返回值
;; ----
;; integer?
;;   -1 表示负数，0 表示零，1 表示正数。

(import (liii check)
        (liii bigint record)
) ;import

(check-set-mode! 'report-failed)

;; 正数
(check (bigint-signum (make-bigint 1 '(1))) => 1)
(check (bigint-signum (make-bigint 1 '(100 200))) => 1)

;; 负数
(check (bigint-signum (make-bigint -1 '(1))) => -1)
(check (bigint-signum (make-bigint -1 '(999 888))) => -1)

;; 零
(check (bigint-signum (make-bigint 0 '())) => 0)

(check-report)
