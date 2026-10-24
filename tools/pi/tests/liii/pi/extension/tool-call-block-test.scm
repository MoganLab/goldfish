(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi extension)
  (liii pi agent tools)
  (liii pi tools)
) ;import

(check-set-mode! 'report-failed)


;; 构造注入了扩展事件桥接与工具执行器的 agent config
(define (make-agent-test-config)
  (list (cons 'emit-extension-event
          (lambda (ev payload)
            (let ((reg (or (current-extension-registry) (default-extension-registry))))
              (pi-emit! reg ev payload))))
    (cons 'execute-tool-call execute-tool-call)))


;; 1. tool_call 拦截：block 阻止执行，直接返回 reason
(let* ((reg (make-extension-registry))
       (tool-called? #f))
  (register-tool! reg
    (make-extension-tool-def "danger_tool"
      "Dangerous tool"
      '()
      (lambda (args)
        (set! tool-called? #t)
        "danger result")))

  (pi-on! reg 'tool_call
    (lambda (payload)
      (if (string=? (cdr (assq 'tool payload)) "danger_tool")
        (list 'block "Policy violation: danger_tool is forbidden")
        'continue)))

  (with-extension-registry reg
    (lambda ()
      (let* ((call '((id . "call_danger")
                     (name . "danger_tool")
                     (arguments . ())))
             (msg (pi-execute-one-tool-call call #f 10 (make-agent-test-config))))
        (check (pi-tool-result-message-is-error msg) => #t)
        (check (pi-extract-tool-result-text msg) => "Policy violation: danger_tool is forbidden")
        (check tool-called? => #f)))))

;; 2. tool_call 拦截：input override 改写参数后正常执行
(let* ((reg (make-extension-registry))
       (executed-input #f))
  (register-tool! reg
    (make-extension-tool-def "echo_tool"
      "Echo tool"
      '()
      (lambda (args)
        (set! executed-input args)
        "ok")))

  (pi-on! reg 'tool_call
    (lambda (payload)
      (if (string=? (cdr (assq 'tool payload)) "echo_tool")
        (list 'override '((text . "overridden text")))
        'continue)))

  (with-extension-registry reg
    (lambda ()
      (let* ((call '((id . "call_echo")
                     (name . "echo_tool")
                     (arguments . ((text . "original text")))))
             (msg (pi-execute-one-tool-call call #f 10 (make-agent-test-config))))
        (check (pi-tool-result-message-is-error msg) => #f)
        (check (cdr (assq 'text executed-input)) => "overridden text")))))

(check-report)
