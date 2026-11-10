;; tool-call-block
;; 验证扩展监听器阻断工具调用时的执行结果。
;;
;; 语法
;; ----
;; (pi-emit! target event-type payload)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 事件所属的扩展对象。
;;
;; event-type : (or symbol string)
;; 事件名称。
;;
;; payload : any
;; 传给监听器的数据。
;;
;; 返回值
;; ----
;; alist
;; 包含 action 与 payload 字段的事件派发结果。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi ai message)
  (liii pi code core extensions)
  (liii pi agent tools)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)


;; 构造注入了扩展事件桥接与工具执行器的 agent config

(define (make-agent-test-config)
  (list
    (cons 'emit-extension-event
      (lambda (ev payload)
        (let ((reg (or (current-extension-registry) (default-extension-registry))))
          (pi-emit! reg ev payload)
        ) ;let
      ) ;lambda
    ) ;cons
    (cons 'execute-tool-call execute-tool-call)
  ) ;list
) ;define


;; 1. tool_call 拦截：block 阻止执行，直接返回 reason
(let* ((reg (make-extension-registry)) (tool-called? #f))
  (register-tool! reg
    (make-extension-tool-def "danger_tool"
      "Dangerous tool"
      '()
      (lambda (args) (set! tool-called? #t) "danger result")
    ) ;make-extension-tool-def
  ) ;register-tool!

  (pi-on! reg
    'tool_call
    (lambda (payload)
      (if (string=? (cdr (assq 'tool payload)) "danger_tool")
        (list 'block "Policy violation: danger_tool is forbidden")
        'continue
      ) ;if
    ) ;lambda
  ) ;pi-on!

  (with-extension-registry reg
    (lambda ()
      (let* ((call '((id . "call_danger") (name . "danger_tool") (arguments)))
             (msg (pi-execute-one-tool-call call #f 10 (make-agent-test-config)))
            ) ;
        (check (pi-tool-result-message-is-error msg) => #t)
        (check (pi-extract-tool-result-text msg)
          =>
          "Policy violation: danger_tool is forbidden"
        ) ;check
        (check tool-called? => #f)
      ) ;let*
    ) ;lambda
  ) ;with-extension-registry
) ;let*

;; 2. tool_call 拦截：input override 改写参数后正常执行
(let* ((reg (make-extension-registry)) (executed-input #f))
  (register-tool! reg
    (make-extension-tool-def "echo_tool"
      "Echo tool"
      '()
      (lambda (args) (set! executed-input args) "ok")
    ) ;make-extension-tool-def
  ) ;register-tool!

  (pi-on! reg
    'tool_call
    (lambda (payload)
      (if (string=? (cdr (assq 'tool payload)) "echo_tool")
        (list 'override '((text . "overridden text")))
        'continue
      ) ;if
    ) ;lambda
  ) ;pi-on!

  (with-extension-registry reg
    (lambda ()
      (let* ((call '((id . "call_echo")
                     (name . "echo_tool")
                     (arguments (text . "original text")))
             ) ;call
             (msg (pi-execute-one-tool-call call #f 10 (make-agent-test-config)))
            ) ;
        (check (pi-tool-result-message-is-error msg) => #f)
        (check (cdr (assq 'text executed-input)) => "overridden text")
      ) ;let*
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(check-report)
