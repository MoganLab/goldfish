(import (liii check)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; 1. pi-on! 注册事件监听器
(let* ((api (make-extension-api))
       (reg (extension-api-registry api))
       (h1 (lambda (p c) 1))
       (h2 (lambda (p c) 2)))
  (pi-on! api 'session_start h1)
  (pi-on! api 'session_start h2)
  (let ((listeners (extension-registry-get-listeners reg 'session_start)))
    (check (length listeners) => 2)
    (check (eq? (car listeners) h1) => #t)
    (check (eq? (cadr listeners) h2) => #t))
) ;let*

;; 2. pi-on! 传入 string 事件名称自动规范为 symbol
(let* ((api (make-extension-api))
       (reg (extension-api-registry api))
       (h (lambda (p c) 'started)))
  (pi-on! api "agent_start" h)
  (let ((listeners (extension-registry-get-listeners reg 'agent_start)))
    (check (length listeners) => 1)
    (check (eq? (car listeners) h) => #t))
) ;let*

;; 3. pi-on! 直接在 registry 和 context 上注册
(let* ((reg (make-extension-registry))
       (ctx (make-extension-context :registry reg))
       (h-reg (lambda (p c) 'reg))
       (h-ctx (lambda (p c) 'ctx)))
  (pi-on! reg 'tool_result h-reg)
  (pi-on! ctx 'tool_result h-ctx)
  (let ((listeners (extension-registry-get-listeners reg 'tool_result)))
    (check (length listeners) => 2)
    (check (eq? (car listeners) h-reg) => #t)
    (check (eq? (cadr listeners) h-ctx) => #t))
) ;let*

;; 4. 错误校验：handler 必须是 procedure，event 必须是 symbol 或 string
(let ((api (make-extension-api)))
  (check-catch 'type-error (pi-on! api 'session_start "not-a-procedure"))
  (check-catch 'type-error (pi-on! api 12345 (lambda (p c) #t)))
  (check-catch 'type-error (pi-on! "not-an-api" 'session_start (lambda (p c) #t)))
) ;let

(check-report)
