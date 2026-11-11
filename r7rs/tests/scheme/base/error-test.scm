;; error
;; 抛出错误并中断执行。
;;
;;
;; 语法
;; ----
;; (error message)
;; (error message obj ...)
;; (error tag message obj ...)
;;
;; 参数
;; ----
;; message : string?
;; 错误信息。
;; obj ... : 任意类型
;; 额外的错误相关对象（irritants）。
;; tag : symbol?
;; 兼容 s7 的显式错误类型标识。
;;
;; 返回值
;; ----
;; 不返回
;; 抛出错误，不返回正常值。
;;
;; 说明
;; ----
;; 1. 抛出可捕获的错误
;; 2. 通常用于表示程序遇到无法恢复的情况
;; 3. 可用 catch 或 guard 捕获处理

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 1. s7 老签名兼容测试
(check (catch #t (lambda () (error 'test-err) 'unreachable) (lambda args 'caught))
  =>
  'caught
) ;check
(check (catch #t (lambda () (error "message")) (lambda args 'caught-msg))
  =>
  'caught-msg
) ;check
(check-catch 'type-error (error 'type-error "expected integer" "abc"))

;; 2. R7RS 规范测试：通过 guard 捕获并检验 error-object
(define err (guard (ex (else ex)) (error "BOOM!" 1 2 3)))
(check (error-object? err) => #t)
(check (error-object-message err) => "BOOM!")
(check (error-object-irritants err) => '(1 2 3))

(check-report)
