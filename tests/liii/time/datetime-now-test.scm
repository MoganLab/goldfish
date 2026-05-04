(import (liii check) (liii time))

;; datetime-now
;; 返回当前日期时间的向量 [year month day hour minute second microsecond]
;;
;; 返回值
;; ----
;; vector
;; 7 个元素的向量，依次为：年、月、日、时、分、秒、微秒

(let ((t (datetime-now)))
  (check (vector? t) => #t)
  (check (vector-length t) => 7)
  ;; year
  (check (integer? (vector-ref t 0)) => #t)
  (check (> (vector-ref t 0) 2020) => #t)
  ;; month
  (check (integer? (vector-ref t 1)) => #t)
  (check (>= (vector-ref t 1) 1) => #t)
  (check (<= (vector-ref t 1) 12) => #t)
  ;; day
  (check (integer? (vector-ref t 2)) => #t)
  (check (>= (vector-ref t 2) 1) => #t)
  (check (<= (vector-ref t 2) 31) => #t)
  ;; hour
  (check (integer? (vector-ref t 3)) => #t)
  (check (>= (vector-ref t 3) 0) => #t)
  (check (<= (vector-ref t 3) 23) => #t)
  ;; minute
  (check (integer? (vector-ref t 4)) => #t)
  (check (>= (vector-ref t 4) 0) => #t)
  (check (<= (vector-ref t 4) 59) => #t)
  ;; second
  (check (integer? (vector-ref t 5)) => #t)
  (check (>= (vector-ref t 5) 0) => #t)
  (check (<= (vector-ref t 5) 59) => #t)
  ;; microsecond
  (check (integer? (vector-ref t 6)) => #t)
  (check (>= (vector-ref t 6) 0) => #t)
  (check (< (vector-ref t 6) 1000000) => #t)
)

(check-report)
