(import (liii check) (liii par) (liii go))

(check-set-mode! 'report-failed)

;; (liii par) 模块冒烟测试

(define ch (make-chan 3))
(par-for-each (lambda (x) (chan-send! ch (* x 2))) '(1 2 3))

(define total (+ (chan-recv! ch) (chan-recv! ch) (chan-recv! ch)))
(check total => 12)

(check (par-map (lambda (x) (* x 2)) '(1 2 3)) => '(2 4 6))

(check (par-filter (lambda (x) (even? x)) '(1 2 3 4 5 6)) => '(2 4 6))

;; vector 并行过程冒烟测试（详细用例见 tests/liii/par/ 下的专属测试文件）

(vector-par-for-each (lambda (x) x) #(1 2 3))

(check (vector-par-map (lambda (x) (* x 2)) #(1 2 3)) => #(2 4 6))

(check (vector-par-filter even? #(1 2 3 4 5 6)) => #(2 4 6))

(check-report)
