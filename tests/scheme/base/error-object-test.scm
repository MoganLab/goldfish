(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; error-object?/message/irritants
;; 首参为 string 的 error 构造 error object 并 raise；
;; 首参为 symbol 的 (error 'type ...) 保持宿主原语行为。
(check (error-object? (guard (ex (else ex)) (error "boom" 1 2))) => #t)
(check (error-object-message (guard (ex (else ex)) (error "boom" 1 2)))
  =>
  "boom"
) ;check
(check (error-object-irritants (guard (ex (else ex)) (error "boom" 1 2)))
  =>
  '(1 2)
) ;check
(check (error-object-irritants (guard (ex (else ex)) (error "solo")))
  =>
  '()
) ;check
(check (error-object? "boom") => #f)
(check (error-object? 'boom) => #f)
(check (error-object? 42) => #f)
(check (error-object? '(boom)) => #f)
;; s7 惯用法保持：guard 收到 message 本身
(check (guard (ex (else ex)) (error 'test-error "message")) => "message")
(check (error-object? (guard (ex (else ex)) (error 'test-error "m"))) => #f)
;; raise 原样透传任意对象
(check (guard (ex (else (error-object? ex))) (raise 42)) => #f)
(check (guard (ex (else ex)) (raise "s")) => "s")
;; with-exception-handler 收到对象本身
(check (with-exception-handler (lambda (e) (error-object-message e))
         (lambda () (error "wired" 'x))
       ) ;with-exception-handler
  =>
  "wired"
) ;check
;; 非 error object 上取 message/irritants 即报错
(check (guard (ex (else 'trapped)) (error-object-message 42)) => 'trapped)
(check (guard (ex (else 'trapped)) (error-object-irritants "s")) => 'trapped)
(check-report)
