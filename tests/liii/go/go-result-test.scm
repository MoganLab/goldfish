(import (liii check) (liii go) (liii base64))

(check-set-mode! 'report-failed)

;; go-result
;; 将代码调度到后台 worker 线程执行，并返回一个结果 channel：任务结束（无论正常
;; 返回还是抛出异常）后都会向该 channel 送入恰好一个结果对象，接收端不会因任务
;; 异常而永久死等。
;;
;; 语法
;; ----
;; (go-result (captured-var ...) body ...)
;; (go-result body ...)
;;
;; 参数
;; ----
;; captured-var : symbol
;; 需要从当前环境捕获传入 worker 的变量名（与 go 相同，只支持可序列化类型）。
;;
;; body : any
;; 在 worker 会话中执行的代码。
;;
;; 返回值
;; ----
;; channel（缓冲 1）：任务完成后可收到恰好一个结果对象：
;;   (ok value)        正常返回，value 为 body 的值
;;   (error tag args)  body 抛出异常，tag 为错误符号，args 为错误参数列表
;;
;; 说明
;; ----
;; 1. 结果 channel 缓冲为 1，即使无人接收，worker 也不会被阻塞。
;; 2. body 的返回值与 error 参数必须是可序列化类型；序列化失败时错误输出到 stderr。
;; 3. 可配合 go-result-recv! 直接取值，并在任务出错时于接收端重抛原异常。

;; 1. 正常返回：结果 channel 收到 (ok value)

(define ch1 (go-result () (+ 1 2)))
(check (chan-recv! ch1 2000) => '(ok 3))

;; 2. 捕获变量传入 worker

(define x 10)

(define ch2 (go-result (x) (* x x)))
(check (chan-recv! ch2 2000) => '(ok 100))

;; 3. 无捕获列表的简写形式（首参数非 list 时整体视为 body，与 go 宏一致）

(define ch3 (go-result 42))
(check (chan-recv! ch3 2000) => '(ok 42))

;; 4. 异常传递：接收端收到 (error tag args) 而非死等

(define ch4 (go-result () (error 'test-error "boom" 42)))
(check (chan-recv! ch4 2000) => '(error test-error ("boom" 42)))

;; 5. 返回复杂可序列化结构

(define ch5 (go-result () (list 1 "two" #(3))))
(check (chan-recv! ch5 2000) => '(ok (1 "two" #(3))))

;; 6. 异常任务不会拖垮线程池，后续任务照常执行

(define ch6 (go-result () 7))
(check (chan-recv! ch6 2000) => '(ok 7))

;; 7. worker 自动 import 主会话已加载的库（worker 前导代码）
;; (liii base64) 不在 worker 的 boot.scm 中，只能由前导 import 引入

(define ch7 (go-result () (string-base64-encode "hi")))
(check (chan-recv! ch7 2000) => '(ok "aGk="))

(check-report)
