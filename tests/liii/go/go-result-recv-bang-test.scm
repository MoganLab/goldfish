(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; go-result-recv!
;; 从 go-result 返回的结果 channel 接收并拆包：任务正常时直接返回 body 的值；
;; 任务抛异常时在接收端重新抛出原异常（保留原错误 tag，可用 check-catch 捕获）。
;;
;; 语法
;; ----
;; (go-result-recv! result-ch)
;; (go-result-recv! result-ch timeout-ms)
;;
;; 参数
;; ----
;; result-ch : channel
;; go-result 返回的结果 channel。
;;
;; timeout-ms : integer
;; 可选，超时毫秒数；到期仍未收到结果则抛出 'timeout-error。
;;
;; 返回值
;; ----
;; 任务 body 的返回值。
;;
;; 异常
;; ----
;; 1. 任务出错时重抛原异常（原 tag 与原参数）。
;; 2. 超时抛出 'timeout-error。
;; 3. 收到非法结果对象时抛出 'type-error。

;; 1. 正常路径直接取值
(check (go-result-recv! (go-result () (+ 20 22))) => 42)

;; 2. 捕获变量

(define y 5)
(check (go-result-recv! (go-result (y) (* y y))) => 25)

;; 3. 异常重抛：保留原 tag 与原参数
(check-catch 'type-error
  (go-result-recv! (go-result () (error 'type-error "not a number" 'sym)))
) ;check-catch

;; 4. 运行时错误（除零）同样重抛
(check-catch 'division-by-zero (go-result-recv! (go-result () (/ 1 0))))

;; 5. 超时：worker 阻塞时接收端按 timeout 抛错而非死等

(define blocker (make-chan 1))

(define slow-ch (go-result (blocker) (chan-recv! blocker)))
(check-catch 'timeout-error (go-result-recv! slow-ch 100))

;; 释放被阻塞的 worker，并确认任务最终仍能完成
(chan-send! blocker 'go)
(check (go-result-recv! slow-ch 2000) => 'go)

(check-report)
