;; decimal-min
;; 返回两个 decimal 中较小的一个。
;;
;; 语法
;; ----
;; (decimal-min a b)
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
;;   数值较小的 decimal；数值相等时返回第二个参数。
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

;; 1.2 和 1.20 数值相等，decimal<? 返回 #f，所以返回第二个参数
(check (decimal->string (decimal-min (d "1.2") (d "1.20"))) => "1.20")
(check (decimal-scale (decimal-min (d "1.2") (d "1.20"))) => 2)

(check (decimal->string (decimal-min (d "-1") (d "0.5"))) => "-1")

(check-catch 'type-error (decimal-min (d "1") 1))

(check-report)
