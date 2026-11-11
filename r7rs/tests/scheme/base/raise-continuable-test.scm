;; raise-continuable
;; 引发可继续异常，若异常处理器正常返回，将其返回值传回调用点。
;;
;;
;; 语法
;; ----
;; (raise-continuable obj)
;;
;; 参数
;; ----
;; obj : 任意对象
;; 传递给当前异常处理器的异常对象。
;;
;; 返回值
;; ----
;; any
;; 当前异常处理器正常返回时的值。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 1. 基本可继续调用，返回处理器的计算值
(define result1
  (with-exception-handler
    (lambda (con)
      (cond ((string? con) (string-append con "!"))
            (else 'other)))
    (lambda ()
      (string-append "hello " (raise-continuable "world")))))
(check result1 => "hello world!")

;; 2. 多次连续触发可继续异常
(define result2
  (with-exception-handler
    (lambda (x) (* x 2))
    (lambda ()
      (+ (raise-continuable 3) (raise-continuable 4)))))
(check result2 => 14)

(check-report)
