;; decimal<=?
;; 判断第一个 decimal 是否小于等于第二个。
;;
;; 语法
;; ----
;; (decimal<=? a b)
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
;; boolean?
;;   如果 a <= b，则返回 #t，否则返回 #f。
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

(check-true (decimal<=? (d "2.0") (d "2")))
(check-false (decimal<=? (d "2.1") (d "2")))

(check-catch 'type-error (decimal<=? 1 (d "1")))

(check-report)
