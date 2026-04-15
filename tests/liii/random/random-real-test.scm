(import (liii random) (liii check))


(check-set-mode! 'report-failed)


;; random-real
;; 生成 (0, 1) 范围内的随机实数。
;;
;; 语法
;; ----
;; (random-real)
;;
;; 返回值
;; ----
;; real
;; 返回 (0, 1) 范围内的随机实数，不包含 0 和 1。
;;
;; 示例
;; ----
;; (random-real)  => (0, 1) 之间的随机实数，如 0.34567
;;
;; 注意
;; ----
;; 多次调用返回不同的值（概率极高）。


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
