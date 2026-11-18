;; make-random-bigint
;; 构造指定比特长度的随机正 bigint。
;;
;; 语法
;; ----
;; (make-random-bigint num-bits)
;;
;; 参数
;; ----
;; num-bits : integer?
;;   要生成的比特长度，必须为正整数。
;;
;; 返回值
;; ----
;; bigint?
;;   指定比特长度的随机正 bigint。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数类型错误时抛出 type-error；num-bits 非正时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define n1 (make-random-bigint 1))
(define n8 (make-random-bigint 8))
(define n32 (make-random-bigint 32))

(check (bigint->string n1) => "1")
(check-true (bigint-positive? n8))
(check-true (bigint-positive? n32))
(check (bigint-bit-length n8) => 8)
(check (bigint-bit-length n32) => 32)

(check-catch 'type-error (make-random-bigint "8"))
(check-catch 'type-error (make-random-bigint 1.5))
(check-catch 'value-error (make-random-bigint 0))
(check-catch 'value-error (make-random-bigint -1))

(check-report)
