(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; syntactic-closure?
;; 判断一个对象是否为句法闭包。
;;
;; 语法
;; ----
;; (syntactic-closure? x)
;;
;; 参数
;; ----
;; x : any
;; 待判定的对象。
;;
;; 返回值
;; ------
;; boolean
;; 当 x 是由 make-syntactic-closure 创建的句法闭包时返回 #t，否则返回 #f。

;; 1. 句法闭包返回 #t
(check-true (syntactic-closure? (make-syntactic-closure (curlet) '() 'x)))
(check-true (syntactic-closure? (make-syntactic-closure (rootlet) '(a) '(+ 1 2))))

;; 2. 其他对象返回 #f
(check-false (syntactic-closure? 'x))
(check-false (syntactic-closure? 42))
(check-false (syntactic-closure? "str"))
(check-false (syntactic-closure? '(a b)))
(check-false (syntactic-closure? #(1 2)))
(check-false (syntactic-closure? #t))
(check-false (syntactic-closure? '()))
(check-false (syntactic-closure? car))

(check-report)
