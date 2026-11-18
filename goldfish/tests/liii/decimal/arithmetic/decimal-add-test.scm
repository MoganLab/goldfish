;; decimal-add
;; 计算两个 decimal 的和。
;;
;; 语法
;; ----
;; (decimal-add a b)
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
;;   a 与 b 的和，scale 取两操作数中较大者。
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

(check (decimal->string (decimal-add (d "1.2") (d "0.03"))) => "1.23")
(check (decimal->string (decimal-add (d "-1.2") (d "0.2"))) => "-1.0")

(check-catch 'type-error (decimal-add 1 (d "1")))

(check-report)
