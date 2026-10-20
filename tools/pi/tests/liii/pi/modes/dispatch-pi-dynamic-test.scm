(import (liii check) (liii pi args) (liii pi modes) (liii pi extension))

(check-set-mode! 'report-failed)

;; 1. 动态 Local Flow：无需 API Key，优先命中执行
(let* ((reg (make-extension-registry))
       (local-executed #f)
       (flow (make-extension-flow-def 'test-local-flow
               (lambda (parsed) (pi-args-ref parsed 'my-local-cmd))
               (lambda (parsed cfg mode initial-msg) (set! local-executed #t) 123)
               :local? #t
             ) ;make-extension-flow-def
       ) ;flow
      ) ;
  (register-flag! reg
    (make-extension-flag-def "--my-local-cmd" "Local cmd" :key 'my-local-cmd)
  ) ;register-flag!
  (register-flow! reg flow)
  (with-extension-registry reg
    (lambda ()
      (let ((parsed (parse-pi-args '("--my-local-cmd"))))
        ;; local flow 在没有任何 provider API key 的情况下也能成功执行并返回退出码 123
        (check (dispatch-pi parsed 'print "" '()) => 123)
        (check local-executed => #t)
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let*

;; 2. 动态 Session Flow：通过认证后命中执行
(let* ((reg (make-extension-registry))
       (session-executed #f)
       (flow (make-extension-flow-def 'test-custom-session
               (lambda (parsed) (pi-args-ref parsed 'custom-agent))
               (lambda (parsed cfg mode initial-msg) (set! session-executed #t) 456)
               :local? #f
             ) ;make-extension-flow-def
       ) ;flow
      ) ;
  (register-flag! reg
    (make-extension-flag-def "--custom-agent" "Custom agent" :key 'custom-agent)
  ) ;register-flag!
  (register-config-augmenter! reg
    (lambda (cfg parsed) (cons '(api-key . "dummy-token") cfg))
  ) ;register-config-augmenter!
  (register-flow! reg flow)
  (with-extension-registry reg
    (lambda ()
      (let ((parsed (parse-pi-args '("--custom-agent" "hello"))))
        (check (dispatch-pi parsed 'print "hello" '()) => 456)
        (check session-executed => #t)
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(check-report)
