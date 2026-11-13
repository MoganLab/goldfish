;; strip-leading-zeros
;; 去除 magnitude 数组的前导零。
;;
;; 语法
;; ----
;; (strip-leading-zeros mag)
;;
;; 参数
;; ----
;; mag : list?
;;   magnitude 数组（大端序）。
;;
;; 返回值
;; ----
;; list?
;;   去除前导零后的数组，空数组或全零返回空。

(import (liii check)
        (liii bigint record)
) ;import

(check-set-mode! 'report-failed)

;; 无前导零
(check (strip-leading-zeros '(1 2 3)) => '(1 2 3))

;; 有前导零
(check (strip-leading-zeros '(0 0 1 2 3)) => '(1 2 3))
(check (strip-leading-zeros '(0 0 0 100)) => '(100))

;; 全零返回空
(check (strip-leading-zeros '(0 0 0)) => '())
(check (strip-leading-zeros '(0)) => '())

;; 空数组
(check (strip-leading-zeros '()) => '())

;; 单个非零
(check (strip-leading-zeros '(42)) => '(42))

(check-report)
