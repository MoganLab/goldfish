(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; raise
;; 抛出异常。
;;
;; 语法
;; ----
;; (raise obj)
;;
;; 参数
;; ----
;; obj : 任意对象
;; 要抛出的异常对象。
;;
;; 返回值
;; ------
;; 不返回，直接抛出异常。

;; 测试抛出符号
(check-catch #t (raise 'my-error))

;; 测试抛出字符串
(check-catch #t (raise "error message"))

;; 测试抛出列表
(check-catch #t (raise '(error type "message")))

;; 测试 guard 捕获 raise
(let ((result (guard (ex (else ex))
                (raise 'test-error)
                'unreachable)))
  (check (eq? result 'test-error) => #t))

;; 测试 raise 后代码不执行
(let ((executed #f))
  (catch #t
    (lambda ()
      (raise 'error)
      (set! executed #t))
    (lambda args 'caught))
  (check executed => #f))

(check-report)
