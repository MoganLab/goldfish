;; make-decimal
;; 创建 decimal 并规范化：去除非标度值的尾随零。
;;
;; 语法
;; ----
;; (make-decimal unscaled scale)
;;
;; 参数
;; ----
;; unscaled : bigint?
;;   非标度值。
;; scale : integer?
;;   标度。
;;
;; 返回值
;; ----
;; decimal?
;;   规范化后的 decimal（尾随零与 scale 同步缩减）。
;;
;; 错误处理
;; ----
;; type-error
;;   当 unscaled 不是 bigint 或 scale 不是整数时抛出。

(import (liii check)
        (liii decimal)
        (liii decimal record)
        (liii bigint)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)

;; 基本构造
(check (decimal-scale (make-decimal (string->bigint "123") 2)) => 2)
(check (bigint->string (decimal-unscaled (make-decimal (string->bigint "123") 2))) => "123")

;; 尾随零规范化：12300 scale=4 等价于 123 scale=2
(check (decimal-scale (make-decimal (string->bigint "12300") 4)) => 2)
(check (bigint->string (decimal-unscaled (make-decimal (string->bigint "12300") 4))) => "123")

;; 多重尾随零
(check (decimal-scale (make-decimal (string->bigint "120000") 6)) => 2)
(check (bigint->string (decimal-unscaled (make-decimal (string->bigint "120000") 6))) => "12")

;; 零统一为 scale 0
(check (decimal-scale (make-decimal bigint-zero 5)) => 0)
(check (bigint->string (decimal-unscaled (make-decimal bigint-zero 5))) => "0")

;; 负数
(check (decimal-scale (make-decimal (string->bigint "-120") 3)) => 2)
(check (bigint->string (decimal-unscaled (make-decimal (string->bigint "-120") 3))) => "-12")

;; scale 为 0 或负数时不规范化
(check (decimal-scale (make-decimal (string->bigint "120") 0)) => 0)
(check (decimal-scale (make-decimal (string->bigint "120") -1)) => -1)

(check-catch 'type-error (make-decimal 123 2))
(check-catch 'type-error (make-decimal (string->bigint "123") 1.5))

(check-report)
