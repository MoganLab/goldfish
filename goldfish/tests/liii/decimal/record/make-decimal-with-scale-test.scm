;; make-decimal-with-scale
;; 按指定 scale 创建 decimal，不做尾随零规范化。
;;
;; 语法
;; ----
;; (make-decimal-with-scale unscaled scale)
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
;;   按给定 scale 构造的 decimal（零除外，零保留给定 scale）。
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

;; 保留给定 scale，不做规范化
(check (decimal-scale (make-decimal-with-scale (string->bigint "12300") 4)) => 4)
(check (bigint->string (decimal-unscaled (make-decimal-with-scale (string->bigint "12300") 4))) => "12300")

;; 零保留给定 scale（与 make-decimal 不同）
(check (decimal-scale (make-decimal-with-scale bigint-zero 5)) => 5)

;; 基本构造
(check (decimal-scale (make-decimal-with-scale (string->bigint "123") 2)) => 2)
(check (decimal-scale (make-decimal-with-scale (string->bigint "120") 0)) => 0)
(check (decimal-scale (make-decimal-with-scale (string->bigint "120") -2)) => -2)

;; 负数
(check (decimal-scale (make-decimal-with-scale (string->bigint "-123") 3)) => 3)
(check (bigint->string (decimal-unscaled (make-decimal-with-scale (string->bigint "-123") 3))) => "-123")

(check-catch 'type-error (make-decimal-with-scale "123" 2))
(check-catch 'type-error (make-decimal-with-scale (string->bigint "123") "2"))

(check-report)
