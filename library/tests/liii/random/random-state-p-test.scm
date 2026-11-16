;; random-state?
;; 判断对象是否为随机数状态对象（random-state）。
;;
;; 语法
;; ----
;; (random-state? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意 Scheme 对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果 obj 是 random-state 对象则返回 #t，否则返回 #f。

(import (scheme base) (liii random) (liii check))

(check-set-mode! 'report-failed)

;; random-state 对象返回 #t
(check-true (random-state? (random-state)))
(check-true (random-state? (random-state 1234)))
(check-true (random-state? (random-state 1234 5678)))

;; 非 random-state 对象返回 #f
(check-false (random-state? 1234))
(check-false (random-state? '(1234 5678)))
(check-false (random-state? "random-state"))
(check-false (random-state? #f))
(check-false (random-state? '()))
(check-false (random-state? (make-random-source)))

(check-report)
