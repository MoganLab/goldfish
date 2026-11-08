;; promise?
;; 判断对象是否为 promise。
;;
;; 语法
;; ----
;; (promise? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean?
;; obj 是 delay / delay-force / make-promise 产生的 promise 时为 #t，否则 #f。
;;
;; 说明
;; ----
;; R7RS 4.2.5：promise 由 delay、delay-force 或 make-promise 构造。
;; 其他类型（数值、过程、列表等）均不是 promise。
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (scheme lazy) (scheme base))

(check-set-mode! 'report-failed)

;; 1. 构造来源识别
(check (promise? (delay 1)) => #t)
(check (promise? (delay-force (delay 1))) => #t)
(check (promise? (make-promise 1)) => #t)

;; 2. 非对象类型
(check (promise? 1) => #f)
(check (promise? "s") => #f)
(check (promise? 'sym) => #f)
(check (promise? '(1 2)) => #f)
(check (promise? #t) => #f)
(check (promise? #f) => #f)
(check (promise? (lambda () 1)) => #f)
(check (promise? (vector 1 2)) => #f)
(check (promise? (current-output-port)) => #f)

;; 3. force 后仍是 promise
(define pr (delay 1))
(force pr)
(check (promise? pr) => #t)

(check-report)
