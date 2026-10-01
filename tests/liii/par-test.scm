(import (liii check) (liii par) (liii go))

(check-set-mode! 'report-failed)

;; (liii par) 模块冒烟测试

(define ch (make-chan 3))
(par-for-each (lambda (x) (chan-send! ch (* x 2))) '(1 2 3))

(define total (+ (chan-recv! ch) (chan-recv! ch) (chan-recv! ch)))
(check total => 12)

(check-report)
