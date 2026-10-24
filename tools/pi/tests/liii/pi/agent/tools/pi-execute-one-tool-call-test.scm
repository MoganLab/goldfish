(import (liii check)
  (liii string)
  (liii pi agent tools)
  (liii pi ai message)
  (liii pi code tools)
)

(check-set-mode! 'report-failed)


;; 构造注入了工具执行器的 agent config
(define (make-agent-test-config)
  (list (cons 'execute-tool-call execute-tool-call)))


;; 1. 执行普通工具（如 bash），验证事件发射与结果构造
(let* ((evs '())
       (mock-emit (lambda (ev-type . args)
                    (set! evs (cons (cons ev-type args) evs))))
       (call (list (cons 'id "call_bash_1")
                   (cons 'name "bash")
                   (cons 'arguments (list (cons "command" "echo hello-agent-tool")))))
       (msg (pi-execute-one-tool-call call mock-emit 10 (make-agent-test-config))))
  (check-true (pi-tool-result-message? msg))
  (check (pi-tool-result-message-role msg) => "toolResult")
  (check (pi-tool-result-message-tool-call-id msg) => "call_bash_1")
  (check (pi-tool-result-message-tool-name msg) => "bash")
  (let ((content (pi-tool-result-message-content msg)))
    (check-true (pair? content))
    (check (pi-text-block-text (car content)) => "hello-agent-tool\n"))
  ;; 验证发射的事件序列包含 start 与 end
  (let ((types (map car (reverse evs))))
    (check-true (pair? (member 'tool_execution_start types)))
    (check-true (pair? (member 'tool_execution_end types)))
    (check-true (pair? (member 'message_start types)))
    (check-true (pair? (member 'message_end types)))))

;; 2. 传递 opt-config 参数，确保正常执行不崩溃
(let* ((call (list (cons 'id "call_bash_2")
                   (cons 'name "bash")
                   (cons 'arguments (list (cons "command" "echo config-ok")))))
       (cfg '((provider . "google") (timeout . 10)))
       (msg (pi-execute-one-tool-call call #f 10 (cons (cons 'execute-tool-call execute-tool-call) cfg))))
  (check-true (pi-tool-result-message? msg))
  (let ((content (pi-tool-result-message-content msg)))
    (check (pi-text-block-text (car content)) => "config-ok\n")))

(check-report)
