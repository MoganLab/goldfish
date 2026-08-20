(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; with-exception-handler
;; 安装异常处理器并求值 thunk。
;;
;; 语法
;; ----
;; (with-exception-handler handler thunk)
;;
;; 参数
;; ----
;; handler : 接收被抛出对象的过程
;; thunk   : 被求值的过程

;; thunk 正常返回时返回其结果
(check (with-exception-handler (lambda (e) e) (lambda () 42)) => 42)

;; thunk 抛出时调用 handler
(check (with-exception-handler
         (lambda (e) (list 'handled e))
         (lambda () (raise 'oops)))
       => '(handled oops))

;; 与 guard 的抛出对象语义一致
(check (with-exception-handler
         (lambda (e) e)
         (lambda () (raise 'test-error)))
       => 'test-error)

(check-report)
