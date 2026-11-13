;; bigint-normalize
;; 规范化 bigint，去除前导零并调整符号。
;;
;; 语法
;; ----
;; (bigint-normalize n)
;;
;; 参数
;; ----
;; n : bigint?
;;   要规范化的 bigint。
;;
;; 返回值
;; ----
;; bigint?
;;   规范化后的新 bigint。

(import (liii check)
        (liii bigint record)
) ;import

(check-set-mode! 'report-failed)

;; 已经规范的数
(let ((n (make-bigint 1 '(123))))
  (let ((norm (bigint-normalize n)))
    (check (bigint-signum norm) => 1)
    (check (bigint-mag norm) => '(123))
  ) ;let
) ;let

;; 零始终规范
(let ((z (make-bigint 0 '())))
  (let ((norm (bigint-normalize z)))
    (check (bigint-signum norm) => 0)
    (check (bigint-mag norm) => '())
  ) ;let
) ;let

(check-report)
