;; BIGINT-BASE
;; 基数常量，值为 2^32。
;;
;; 语法
;; ----
;; BIGINT-BASE
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;;   基数值 4294967296。

(import (liii check)
        (liii bigint record)
) ;import

(check-set-mode! 'report-failed)

;; BIGINT-BITS
;; 每个数字的位数，值为 32。

;; 验证常量值
(check BIGINT-BASE => 4294967296)
(check BIGINT-BITS => 32)

;; 验证关系
(check BIGINT-BASE => (expt 2 BIGINT-BITS))

(check-report)
