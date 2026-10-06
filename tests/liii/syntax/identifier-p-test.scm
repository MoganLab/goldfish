(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; identifier?
;; 判断一个对象是否为标识符。
;;
;; 语法
;; ----
;; (identifier? x)
;;
;; 参数
;; ----
;; x : any
;; 待判定的对象。
;;
;; 返回值
;; ------
;; boolean
;; 当 x 是符号、或逐层剥开后 expr 为符号的句法闭包时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 在卫生宏系统中，标识符可以是裸符号，也可以是被句法闭包包装的符号；
;; identifier? 会自动穿透任意层数的句法闭包包装进行判定。

;; 1. 裸符号是标识符
(check-true (identifier? 'x))
(check-true (identifier? 'lambda))

;; 2. 包装符号的句法闭包是标识符
(check-true (identifier? (make-syntactic-closure (curlet) '() 'x)))

;; 3. 多层嵌套包装的符号仍是标识符
(let* ((inner (make-syntactic-closure (curlet) '() 'x))
       (outer (make-syntactic-closure (curlet) '() inner)))
  (check-true (identifier? outer))
) ;let*

;; 4. 包装了非符号的闭包不是标识符
(check-false (identifier? (make-syntactic-closure (curlet) '() '(+ 1 2))))
(check-false (identifier? (make-syntactic-closure (curlet) '() 42)))

;; 5. 其他对象不是标识符
(check-false (identifier? 42))
(check-false (identifier? "x"))
(check-false (identifier? '(x)))
(check-false (identifier? #\x))
(check-false (identifier? '()))

(check-report)
