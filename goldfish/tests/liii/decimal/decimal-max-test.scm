;; decimal-max
;; 返回两个 decimal 中较大的一个。
;;
;; 语法
;; ----
;; (decimal-max a b)
;;
;; 参数
;; ----
;; a : decimal?
;;   第一个操作数。
;; b : decimal?
;;   第二个操作数。
;;
;; 返回值
;; ----
;; decimal?
;;   数值较大的 decimal；数值相等时返回第一个参数。
;;
;; 错误处理
;; ----
;; type-error
;;   当任一参数不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

;; 数值相等时返回第一个参数，因此 scale 保留第一个参数的 scale
(check (decimal->string (decimal-max (d "1.2") (d "1.20"))) => "1.2")
(check (decimal-scale (decimal-max (d "1.2") (d "1.20"))) => 1)

(check (decimal->string (decimal-max (d "-1") (d "0.5"))) => "0.5")

(check-catch 'type-error (decimal-max 1 (d "1")))

(check-report)
