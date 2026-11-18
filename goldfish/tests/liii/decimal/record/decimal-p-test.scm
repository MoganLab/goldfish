;; decimal?
;; 判断对象是否为 decimal 记录类型。
;;
;; 语法
;; ----
;; (decimal? obj)
;;
;; 参数
;; ----
;; obj : any
;;   待判断的对象。
;;
;; 返回值
;; ----
;; boolean?
;;   是 decimal 则返回 #t，否则返回 #f。

(import (liii check)
        (liii decimal)
        (liii decimal record)
        (liii bigint)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)

(define (d unscaled scale)
  (make-decimal-with-scale unscaled scale)
) ;define

;; 构造出的对象是 decimal
(check (decimal? (d (string->bigint "123") 2)) => #t)
(check (decimal? (d bigint-zero 0)) => #t)

;; 非 decimal 对象
(check (decimal? 123) => #f)
(check (decimal? "1.23") => #f)
(check (decimal? 'symbol) => #f)
(check (decimal? '()) => #f)
(check (decimal? #t) => #f)

(check-report)
