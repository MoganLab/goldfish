;; pi-execute-one-tool-call-with-status
;; 执行单个工具调用并返回工具结果消息和终止标记状态。
;;
;; 语法
;; ----
;; (pi-execute-one-tool-call-with-status call emit timeout (opt-config #f))
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
;; (values pi-tool-result-message? boolean?)，返回工具结果消息与是否请求终止执行的布尔值。

(import (scheme base)
  (liii check)
  (liii pi agent tools)
  (liii pi ai message)
  (liii pi ai tools record)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正常执行工具调用：返回结果消息且 term? 为 #f
(let* ((mock-exec (lambda (name args . opts) (pi-tool-result "hello world" #f)))
       (cfg (list (cons 'execute-tool-call mock-exec)))
       (call '((id . "call_1") (name . "test_tool") (arguments)))
       (evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
      ) ;
  (let-values (((msg term?) (pi-execute-one-tool-call-with-status call emit 10 cfg)))
    (check-true (pi-tool-result-message? msg))
    (check (pi-tool-result-message-tool-call-id msg) => "call_1")
    (check (pi-text-block-text (car (pi-tool-result-message-content msg)))
      =>
      "hello world"
    ) ;check
    (check term? => #f)
  ) ;let-values
) ;let*

;; 2. 扩展系统拦截并请求终止（terminate）：term? 为 #t
(let* ((mock-ext-emit (lambda (event-type payload)
                        '((action . block)
                          (reason . "blocked and terminate")
                          (terminate . #t))
                      ) ;lambda
       ) ;mock-ext-emit
       (cfg (list (cons 'emit-extension-event mock-ext-emit)))
       (call '((id . "call_2") (name . "blocked_tool") (arguments)))
      ) ;
  (let-values (((msg term?) (pi-execute-one-tool-call-with-status call #f 10 cfg)))
    (check-true (pi-tool-result-message? msg))
    (check (pi-tool-result-message-is-error msg) => #t)
    (check term? => #t)
  ) ;let-values
) ;let*

(check-report)
