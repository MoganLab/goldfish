(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; fiber-yield!
;; 主动让出当前 fiber 的执行权，切换到就绪队列中的下一个协程。
;;
;; 语法
;; ----
;; (fiber-yield!)
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 协程在让出点保存现场（call/cc），再次被调度时从让出点继续。

(define log '())
(spawn-fiber
  (lambda ()
    (set! log (cons 'a1 log))
    (fiber-yield!)
    (set! log (cons 'a2 log))))
(spawn-fiber
  (lambda ()
    (set! log (cons 'b1 log))
    (fiber-yield!)
    (set! log (cons 'b2 log))))
(fiber-scheduler-run!)
;; 验证交错执行
(check (reverse log) => '(a1 b1 a2 b2))

(check-report)
