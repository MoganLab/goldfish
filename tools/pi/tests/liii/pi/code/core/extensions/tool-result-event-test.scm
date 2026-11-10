;; tool-result-event
;; 验证工具执行结果事件的派发行为。
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


;; 1. 工具执行完毕后正常派发 tool_result 事件
(let* ((reg (make-extension-registry)) (observed-results '()))
  (register-tool! reg
    (make-extension-tool-def "math_tool"
      "Math tool"
      '()
      (lambda (args) "result:42")
    ) ;make-extension-tool-def
  ) ;register-tool!

  (pi-on! reg
    'tool_result
    (lambda (payload) (set! observed-results (cons payload observed-results)))
  ) ;pi-on!

  (with-extension-registry reg
    (lambda ()
      (let* ((call '((id . "call_math")
                     (name . "math_tool")
                     (arguments (num . 42))))
             (msg (pi-execute-one-tool-call call #f 10 (make-agent-test-config)))
            ) ;
        (check (length observed-results) => 1)
        (let ((ev (car observed-results)))
          (check (cdr (assq 'name ev)) => "math_tool")
          (check (cdr (assq 'id ev)) => "call_math")
          (check (cdr (assq 'input ev)) => '((num . 42)))
          (check (cdr (assq 'result ev)) => "result:42")
          (check (cdr (assq 'isError ev)) => #f)
        ) ;let
      ) ;let*
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(check-report)
