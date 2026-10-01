(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; go-apply
;; 将 (apply f args) 投递到后台 worker 线程执行，结果按 go-result 协议
;; 送入指定 channel：(ok value) | (error tag args)。
;;
;; 语法
;; ----
;; (go-apply f args ch)
;;
;; 参数
;; ----
;; f : procedure
;; 在 worker 中执行的过程。运行时通过 procedure-source 提取源码运输到 worker，
;; 词法自由变量中的可序列化数据会自动捕获运输，无需显式列出。
;;
;; args : list
;; 传给 f 的参数列表，元素须为可序列化的数据。
;;
;; ch : channel
;; 结果 channel，任务结束（无论正常返回还是抛出异常）后收到恰好一个结果对象。
;;
;; 返回值
;; ----
;; unspecified

;; 1. 基本调用：结果送入 channel

(define ch1 (make-chan 1))
(go-apply (lambda (x) (* x x)) '(5) ch1)
(check (chan-recv! ch1 2000) => '(ok 25))

;; 2. 多参数调用

(define ch2 (make-chan 1))
(go-apply (lambda (a b) (+ a b)) '(3 4) ch2)
(check (chan-recv! ch2 2000) => '(ok 7))

;; 3. 自动捕获顶层自由变量

(define base 100)

(define ch3 (make-chan 1))
(go-apply (lambda (x) (+ x base)) '(5) ch3)
(check (chan-recv! ch3 2000) => '(ok 105))

;; 4. 自动捕获词法（let 内）自由变量

(define ch4 (make-chan 1))
(let ((offset 7))
  (go-apply (lambda (x) (+ x offset)) '(1) ch4)
) ;let
(check (chan-recv! ch4 2000) => '(ok 8))

;; 5. 异常传递：接收端收到 (error tag args)，可用 go-result-recv! 重抛

(define ch5 (make-chan 1))
(go-apply (lambda (x) (error 'value-error "boom" x)) '(42) ch5)
(check (chan-recv! ch5 2000) => '(error value-error ("boom" 42)))

(define ch6 (make-chan 1))
(go-apply (lambda (x) (error 'value-error "boom")) '(1) ch6)
(check-catch 'value-error (go-result-recv! ch6))

;; 6. 参数类型校验（type-error）

(define ch7 (make-chan 1))
(check-catch 'type-error (go-apply 123 '(1) ch7))
(check-catch 'type-error (go-apply (lambda (x) x) "not-a-list" ch7))
(check-catch 'type-error (go-apply (lambda (x) x) '(1) "not-a-chan"))
(check-catch 'type-error (go-apply display '(1) ch7))

;; 7. 异常任务不会拖垮线程池，后续任务照常执行

(define ch8 (make-chan 1))
(go-apply (lambda (x) (+ x 1)) '(1) ch8)
(check (chan-recv! ch8 2000) => '(ok 2))

(check-report)
