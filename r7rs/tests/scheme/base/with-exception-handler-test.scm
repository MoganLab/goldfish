;; with-exception-handler
;; 安装动态作用域异常处理器并执行 thunk。
;;
;;
;; 语法
;; ----
;; (with-exception-handler handler thunk)
;;
;; 参数
;; ----
;; handler : procedure?
;; 异常处理单参过程。
;; thunk : procedure?
;; 无参过程，在该处理器生效的动态上下文中执行。
;;
;; 返回值
;; ----
;; any
;; thunk 的求值结果（如果无异常）；或可继续异常时由 handler 恢复执行的结果。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 1. 正常执行不引发异常时，原样返回 thunk 的结果
(check (with-exception-handler (lambda (x) 'handled) (lambda () 42)) => 42)

;; 2. 配合 call/cc 跳出异常
(define result
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (x) (k (list 'caught x)))
        (lambda () (+ 1 (raise 'boom)))))))
(check result => '(caught boom))

;; 3. 嵌套 with-exception-handler：外层捕获内层 handler 抛出的异常
(define nested-result
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (x) (k (list 'outer-caught x)))
        (lambda ()
          (with-exception-handler
            (lambda (x) (raise (list 'inner-escalated x)))
            (lambda () (raise 'original-error))))))))
(check nested-result => '(outer-caught (inner-escalated original-error)))

;; 4. 配合 raise-continuable 恢复执行
(define cont-result
  (with-exception-handler
    (lambda (x) (+ x 1))
    (lambda () (+ 10 (raise-continuable 5)))))
(check cont-result => 16)

;; 5. 若 handler 针对非 continuable 异常正常返回，应报错
(check-catch #t
  (with-exception-handler
    (lambda (x) 'ignored)
    (lambda () (raise 'fatal))))

(check-report)
