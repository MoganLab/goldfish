;; bigint-mag
;; 获取 bigint 的 magnitude 数组。
;;
;; 语法
;; ----
;; (bigint-mag n)
;;
;; 参数
;; ----
;; n : bigint?
;;   bigint 记录。
;;
;; 返回值
;; ----
;; list?
;;   magnitude 数组，大端序（最高有效位在前）。

(import (liii check)
        (liii bigint record)
) ;import

(check-set-mode! 'report-failed)

;; 正数
(check (bigint-mag (make-bigint 1 '(1))) => '(1))
(check (bigint-mag (make-bigint 1 '(100 200))) => '(100 200))

;; 负数
(check (bigint-mag (make-bigint -1 '(300))) => '(300))

;; 零
(check (bigint-mag (make-bigint 0 '())) => '())

;; 大数
(check (bigint-mag (make-bigint 1 '(1 2 3 4 5))) => '(1 2 3 4 5))

(check-report)
