;; compose
;; 组合多个一元函数，从右到左依次调用。
;;
;; 语法
;; ----
;; (compose f ...)
;; (compose)
;;
;; 参数
;; ----
;; f : procedure
;; 待组合的函数，按从左到右的顺序给出。
;; 组合结果等价于 (lambda (x) (f (g ... (h x))))，即最右侧的函数最先求值。
;;
;; 返回值
;; ----
;; procedure
;; 返回组合后的新过程。
;;
;; 注意
;; ----
;; 不传入任何函数时，(compose) 返回恒等函数。
;;
;; 错误处理
;; ----
;; 调用组合结果时，若某个函数接受的参数个数不匹配，会抛出参数个数错误。

(import (liii check) (scheme char) (liii base))


(check-set-mode! 'report-failed)


;; 单个函数
(check ((compose (lambda (x) (* x 2))) 21) => 42)

;; 两个函数：从右到左调用
(check ((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 3) => 8)
(check ((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 0) => 2)

;; 三个函数
(check ((compose (lambda (x) (+ x 1))
                    (lambda (x) (* x 10))
                    (lambda (x) (- x 3)))
        5)
  =>
  21
) ;check

;; 与内置函数的组合
(check ((compose number->string (lambda (x) (* x 2))) 21) => "42")
(check ((compose (lambda (x) (string-append x "!")) string-upcase) "hi") => "HI!")

;; 空组合返回恒等函数
(check ((compose) 42) => 42)
(check ((compose) 'sym) => 'sym)

;; 组合结果可以再次组合
(check ((compose (compose (lambda (x) (* x 2)) (lambda (x) (+ x 1)))
                 (lambda (x) (* x 3)))
        2)
  =>
  14
) ;check


(check-report)
