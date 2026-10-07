(import (liii check) (liii par) (liii go))

(check-set-mode! 'report-failed)

;; (liii par) 模块冒烟测试

(define ch (make-chan 3))
(par-for-each (lambda (x) (chan-send! ch (* x 2))) '(1 2 3))

(define total (+ (chan-recv! ch) (chan-recv! ch) (chan-recv! ch)))
(check total => 12)

(check (par-map (lambda (x) (* x 2)) '(1 2 3)) => '(2 4 6))

(check (par-filter (lambda (x) (even? x)) '(1 2 3 4 5 6)) => '(2 4 6))

;; vector 并行过程冒烟测试

(define v-ch (make-chan 4))
(vector-par-for-each (lambda (x) (chan-send! v-ch (* x 2))) #(1 2 3 4))

(define v-res
  (sort! (list (chan-recv! v-ch) (chan-recv! v-ch) (chan-recv! v-ch) (chan-recv! v-ch))
    <
  ) ;sort!
) ;define
(check v-res => '(2 4 6 8))

(check (vector-par-map (lambda (x) (* x 2)) #(1 2 3 4 5 6 7 8))
  =>
  #(2 4 6 8 10 12 14 16)
) ;check

(define factor-res
  (let ((factor 10))
    (vector-par-map (lambda (x) (* x factor)) #(1 2 3))
  ) ;let
) ;define
(check factor-res => #(10 20 30))

(check (vector-par-filter even? #(1 2 3 4 5 6 7 8)) => #(2 4 6 8))

(check-catch 'type-error (vector-par-map display #(1 2 3)))

(check-report)
