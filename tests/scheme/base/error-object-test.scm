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
;; Native keyed errors expose their full exception object to R7RS handlers.
(check (guard (ex (else (error-object-message ex))) (error 'test-error "message")) => "message")
(check (guard (ex (else (error-object-irritants ex))) (error 'test-error "message")) => '("message"))
(check (error-object? (guard (ex (else ex)) (error 'test-error "m"))) => #t)
;; raise 原样透传任意对象
(check (guard (ex (else (error-object? ex))) (raise 42)) => #f)
(check (guard (ex (else ex)) (raise "s")) => "s")
;; with-exception-handler 收到对象本身
(check (call/cc (lambda (escape)
         (with-exception-handler (lambda (e) (escape (error-object-message e)))
           (lambda () (error "wired" 'x)))))
  =>
  "wired"
) ;check
;; 非 error object 上取 message/irritants 即报错
(check (guard (ex (else 'trapped)) (error-object-message 42)) => 'trapped)
(check (guard (ex (else 'trapped)) (error-object-irritants "s")) => 'trapped)
(check-report)
