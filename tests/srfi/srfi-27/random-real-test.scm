;; random-real
;; 生成 (0, 1) 范围内的随机实数。
;;
;; 语法
;; ----
;; (random-real)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; real
;; 返回 (0, 1) 范围内的随机实数，不包含 0 和 1。
;;
;; 注意
;; ----
;; 多次调用返回不同的值（概率极高）。

(import (srfi srfi-27) (liii check))


(check-set-mode! 'report-failed)




(let ((r (random-real)))
  (check (real? r) => #t)
  (check (> r 0) => #t)
  (check (< r 1) => #t)
) ;let


(let ((r1 (random-real)) (r2 (random-real)))
  (check (not (= r1 r2)) => #t)
) ;let


(let ((r (random-real)))
  (check (>= r 0.0) => #t)
  (check (<= r 1.0) => #t)
) ;let


(check-report)
