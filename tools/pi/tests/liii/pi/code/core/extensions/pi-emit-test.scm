(import (liii check)
  (liii error)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

;; 1. 普通生命周期派发 (session_start, agent_start)
(let* ((ctx (make-extension-context :session-id "sess-1"))
       (logs '()))
  (pi-on! ctx 'session_start
    (lambda (payload c)
      (set! logs (cons (list 'session (cdr (assq 'session-id payload))) logs))))
  (pi-on! ctx 'agent_start
    (lambda (payload)
      (set! logs (cons (list 'agent (cdr (assq 'turn payload))) logs))))

  (let ((res1 (pi-emit! ctx 'session_start '((session-id . "sess-1")))))
    (check (cdr (assq 'action res1)) => 'continue)
    (check (cdr (assq 'session-id (cdr (assq 'payload res1)))) => "sess-1"))

  (let ((res2 (pi-emit! ctx 'agent_start '((turn . 1)))))
    (check (cdr (assq 'action res2)) => 'continue)
    (check (cdr (assq 'turn (cdr (assq 'payload res2)))) => 1))

  (check logs => '((agent 1) (session "sess-1")))
) ;let*

;; 2. 缺口四：agent_end 事件断言
(let* ((ctx (make-extension-context :session-id "sess-end"))
       (end-received #f))
  (pi-on! ctx 'agent_end
    (lambda (payload c)
      (set! end-received (list (cdr (assq 'exit-reason payload))
                               (cdr (assq 'total-turns payload))))))

  (let ((res (pi-emit! ctx 'agent_end '((exit-reason . "stop") (total-turns . 3)))))
    (check (cdr (assq 'action res)) => 'continue)
    (check (cdr (assq 'exit-reason (cdr (assq 'payload res)))) => "stop")
    (check (cdr (assq 'total-turns (cdr (assq 'payload res)))) => 3))

  (check end-received => '("stop" 3))
) ;let*

;; 3. 缺口一：tool_call 事件的 block 拦截
;; 3a: 监听器返回 (list 'block "Forbidden tool call")，短路后续监听器
(let* ((ctx (make-extension-context))
       (l2-called? #f))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (if (string=? (cdr (assq 'tool payload)) "bash")
        (list 'block "Forbidden bash command")
        'continue)))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (set! l2-called? #t)))

  (let ((res (pi-emit! ctx 'tool_call '((tool . "bash") (input . ((command . "rm -rf /")))))))
    (check (cdr (assq 'action res)) => 'block)
    (check (cdr (assq 'reason res)) => "Forbidden bash command")
    (check l2-called? => #f))
) ;let*

;; 3b: 监听器返回 (cons 'block "Blocked via pair")
(let* ((ctx (make-extension-context)))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (cons 'block "Blocked via pair")))
  (let ((res (pi-emit! ctx 'tool_call '((tool . "test")))))
    (check (cdr (assq 'action res)) => 'block)
    (check (cdr (assq 'reason res)) => "Blocked via pair"))
) ;let*

;; 3c: 监听器返回 (list 'block) 不带具体原因，使用默认原因
(let* ((ctx (make-extension-context)))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (list 'block)))
  (let ((res (pi-emit! ctx 'tool_call '((tool . "test")))))
    (check (cdr (assq 'action res)) => 'block)
    (check (cdr (assq 'reason res)) => "Tool execution blocked by extension"))
) ;let*

;; 4. 缺口二：tool_call 事件的 override 改写
;; 4a: 监听器返回 (list 'override '((path . "safe.scm")))，改写 input 并继续派发
(let* ((ctx (make-extension-context))
       (l2-seen-input #f))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (if (string=? (cdr (assq 'tool payload)) "read")
        (list 'override '((path . "safe.scm")))
        'continue)))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (set! l2-seen-input (cdr (assq 'input payload)))))

  (let ((res (pi-emit! ctx 'tool_call '((tool . "read") (input . ((path . "secret.scm")))))))
    (check (cdr (assq 'action res)) => 'continue)
    (check (cdr (assq 'input (cdr (assq 'payload res)))) => '((path . "safe.scm")))
    (check l2-seen-input => '((path . "safe.scm"))))
) ;let*

;; 4b: 原 payload 使用 params 字段时 override 同样生效
(let* ((ctx (make-extension-context)))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (list 'override '((query . "goldfish scheme")))))
  (let ((res (pi-emit! ctx 'tool_call '((tool . "grep") (params . ((query . "bad")))))))
    (check (cdr (assq 'action res)) => 'continue)
    (check (cdr (assq 'input (cdr (assq 'payload res)))) => '((query . "goldfish scheme"))))
) ;let*

;; 5. 缺口三：监听器异常隔离
(let* ((ctx (make-extension-context))
       (order '()))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (set! order (cons 1 order))))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (type-error "Listener crashed intentionally")))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      (set! order (cons 3 order))))

  ;; 派发时，即使第二个监听器抛错，自身也不应中断，后续监听器 3 仍应正常执行
  (let ((res (pi-emit! ctx 'tool_call '((tool . "test")))))
    (check (cdr (assq 'action res)) => 'continue)
    (check (reverse order) => '(1 3)))
) ;let*

(check-report)
