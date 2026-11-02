;; pi-execute-one-tool-call
;; 执行单个工具调用并返回工具结果消息。
;;
;; 语法
;; ----
;; (pi-execute-one-tool-call call emit timeout (opt-config #f))
;;
;; 参数
;; ----
;; call : list?，包含 id, name, arguments 的工具调用 alist。
;; emit : (or procedure? #f)，事件派发过程。
;; timeout : real?，工具执行超时时间（秒）。
;; opt-config : (or list? #f)，可选的 Agent 配置 alist。
;;
;; 返回值
;; ----
;; pi-tool-result-message?，工具执行结果消息对象。

(import (liii check)
  (liii string)
  (liii pi agent tools)
  (liii pi ai message)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

;; 构造注入了工具执行器的 agent config

(define (make-agent-test-config)
  (list (cons 'execute-tool-call execute-tool-call))
) ;define

;; 1. 执行普通工具（如 bash），验证事件发射与结果构造
(let* ((evs '())
       (mock-emit
         (lambda (ev-type . args) (set! evs (cons (cons ev-type args) evs)))
       ) ;mock-emit
       (call
         (list (cons 'id "call_bash_1")
           (cons 'name "bash")
           (cons 'arguments (list (cons "command" "echo hello-agent-tool")))
         ) ;list
       ) ;call
       (msg (pi-execute-one-tool-call call mock-emit 10 (make-agent-test-config)))
      ) ;
  (check-true (pi-tool-result-message? msg))
  (check (pi-tool-result-message-role msg) => "toolResult")
  (check (pi-tool-result-message-tool-call-id msg) => "call_bash_1")
  (check (pi-tool-result-message-tool-name msg) => "bash")
  (let ((content (pi-tool-result-message-content msg)))
    (check-true (pair? content))
    (check (pi-text-block-text (car content)) => "hello-agent-tool\n")
  ) ;let
  ;; Bash 执行过程还会发出累计输出快照，end 携带真实执行耗时
  (let ((types (map car (reverse evs))))
    (check-true (pair? (member 'tool_execution_start types)))
    (check-true (pair? (member 'tool_execution_update types)))
    (check-true (pair? (member 'tool_execution_end types)))
    (check-true (pair? (member 'message_start types)))
    (check-true (pair? (member 'message_end types)))
  ) ;let
  (let ((end-ev (assq 'tool_execution_end evs)))
    (check-true (number? (list-ref end-ev 5)))
  ) ;let
) ;let*

;; 2. 传递 opt-config 参数，确保正常执行不崩溃
(let* ((call
         (list (cons 'id "call_bash_2")
           (cons 'name "bash")
           (cons 'arguments (list (cons "command" "echo config-ok")))
         ) ;list
       ) ;call
       (cfg '((provider . "google") (timeout . 10)))
       (msg (pi-execute-one-tool-call call
              #f
              10
              (cons (cons 'execute-tool-call execute-tool-call) cfg)
            ) ;pi-execute-one-tool-call
       ) ;msg
      ) ;
  (check-true (pi-tool-result-message? msg))
  (let ((content (pi-tool-result-message-content msg)))
    (check (pi-text-block-text (car content)) => "config-ok\n")
  ) ;let
) ;let*

(check-report)
