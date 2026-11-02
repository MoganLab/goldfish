;; pi-execute-tool-calls-with-status
;; 批量顺序执行工具调用列表，并返回所有结果消息及批次终止标记。
;;
;; 语法
;; ----
;; (pi-execute-tool-calls-with-status tool-calls emit timeout (opt-config #f))
;;
;; 参数
;; ----
;; tool-calls : list?，待执行的工具调用块列表。
;; emit : (or procedure? #f)，事件派发过程。
;; timeout : real?，工具执行超时时间（秒）。
;; opt-config : (or list? #f)，可选的 Agent 配置 alist。
;;
;; 返回值
;; ----
;; (values list? boolean?)，返回工具结果消息列表与是否包含终止标记的布尔值。

(import (scheme base)
  (liii check)
  (liii pi agent tools)
  (liii pi ai message)
  (liii pi ai tools record)
) ;import

(check-set-mode! 'report-failed)

;; 1. 全部正常执行：返回所有结果消息，batch-term? 为 #f
(let* ((mock-exec (lambda (name args . opts) (pi-tool-result (string-append "res:" name) #f))
       ) ;mock-exec
       (cfg (list (cons 'execute-tool-call mock-exec)))
       (calls (list '((id . "c1") (name . "toolA") (arguments))
                '((id . "c2") (name . "toolB") (arguments))
              ) ;list
       ) ;calls
      ) ;
  (let-values (((msgs term?) (pi-execute-tool-calls-with-status calls #f 10 cfg)))
    (check (length msgs) => 2)
    (check (pi-tool-result-message-tool-name (car msgs)) => "toolA")
    (check (pi-tool-result-message-tool-name (cadr msgs)) => "toolB")
    (check term? => #f)
  ) ;let-values
) ;let*

;; 2. 第一个工具被拦截并终止（terminate）：第二个工具被跳过，batch-term? 为 #t
(let* ((mock-ext-emit
         (lambda (ev-type payload)
           (let ((tool-name (cdr (assoc 'tool payload))))
             (if (equal? tool-name "toolA")
               '((action . block) (reason . "stop batch") (terminate . #t))
               '((action . continue))
             ) ;if
           ) ;let
         ) ;lambda
       ) ;mock-ext-emit
       (cfg (list (cons 'emit-extension-event mock-ext-emit)))
       (calls (list '((id . "c1") (name . "toolA") (arguments))
                '((id . "c2") (name . "toolB") (arguments))
              ) ;list
       ) ;calls
      ) ;
  (let-values (((msgs term?) (pi-execute-tool-calls-with-status calls #f 10 cfg)))
    (check (length msgs) => 2)
    (check (pi-tool-result-message-is-error (car msgs)) => #t)
    (check (pi-tool-result-message-is-error (cadr msgs)) => #t)
    (check
      (pi-text-block-text (car (pi-tool-result-message-content (cadr msgs))))
      =>
      "Tool call skipped: previous tool call terminated the run"
    ) ;check
    (check term? => #t)
  ) ;let-values
) ;let*

(check-report)
