;; BIGINT-BITS
;; 每个数字的位数常量，值为 32。
;;
;; 语法
;; ----
;; BIGINT-BITS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;;   位数值 32。

(import (liii check)
        (liii bigint record)
) ;import

(check-set-mode! 'report-failed)

;; 验证常量值
(check BIGINT-BITS => 32)

;; 验证与 BIGINT-BASE 的关系
(check BIGINT-BASE => (expt 2 BIGINT-BITS))

(check-report)
