;; force
;; 强制求值一个 promise，返回其值；非 promise 参数允许原样返回。
;;
;; 语法
;; ----
;; (force promise)
;;
;; 参数
;; ----
;; promise : any
;; promise 对象；按 R7RS 允许传入非 promise，此时原样返回。
;;
;; 返回值
;; ----
;; any
;; promise 记忆化的值；非 promise 参数原样返回。
;;
;; 说明
;; ----
;; R7RS 4.2.5：force 求值 promise 封装的表达式并缓存结果；
;; 二次 force 同一 promise 直接返回缓存值，不重复求值。
;;
;; 1. 记忆化：表达式只求值一次
;;   (define p (delay (expensive)))  (force p) (force p) ; expensive 只调用一次
;; 2. R7RS 建议实现在 promise 参数已处于已求值状态或非 promise 时，
;;   (force promise) 以尾调用返回
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (scheme lazy) (scheme base))

(check-set-mode! 'report-failed)

;; 1. 基本求值
(check (force (delay (+ 1 2))) => 3)
(check (force (delay 'a)) => 'a)
(check (force (delay "text")) => "text")

;; 2. 非 promise 参数原样返回（R7RS 允许）
(check (force 42) => 42)
(check (force 'sym) => 'sym)
(check (force (list 1 2 3)) => '(1 2 3))
(check (force #f) => #f)

;; 3. 记忆化：副作用只发生一次
(define f-count 0)
(define f-p (delay (begin (set! f-count (+ f-count 1)) f-count)))
(check (force f-p) => 1)
(check (force f-p) => 1)
(check (force f-p) => 1)
(check f-count => 1)

;; 4. 配合 make-promise
(check (force (make-promise 9)) => 9)
(check (force (make-promise (delay 11))) => 11)

;; 5. promise 求值结果的类型不受限制（含多值）
(check (call-with-values (lambda () (force (delay (values 1 2)))) list)
  => '(1 2))

(check-report)
