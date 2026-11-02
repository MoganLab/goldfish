;; pi-execute-tool-calls
;; 批量顺序执行工具调用列表，并返回所有工具调用的结果消息列表。
;;
;; 语法
;; ----
;; (pi-execute-tool-calls tool-calls emit timeout (opt-config #f))
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
;; list?，工具执行结果消息（pi-tool-result-message）列表。

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

;; 1. 执行批量工具调用
(let* ((calls
         (list
           (list (cons 'id "call_1")
             (cons 'name "bash")
             (cons 'arguments (list (cons "command" "echo batch-1")))
           ) ;list
           (list (cons 'id "call_2")
             (cons 'name "bash")
             (cons 'arguments (list (cons "command" "echo batch-2")))
           ) ;list
         ) ;list
       ) ;calls
       (res (pi-execute-tool-calls calls #f 10 (make-agent-test-config)))
      ) ;
  (check (length res) => 2)
  (let* ((m1 (car res))
         (m2 (cadr res))
         (c1 (pi-tool-result-message-content m1))
         (c2 (pi-tool-result-message-content m2))
        ) ;
    (check (pi-text-block-text (car c1)) => "batch-1\n")
    (check (pi-text-block-text (car c2)) => "batch-2\n")
  ) ;let*
) ;let*

;; 2. 传递 config 时正常批量执行
(let* ((calls
         (list
           (list (cons 'id "call_cfg")
             (cons 'name "bash")
             (cons 'arguments (list (cons "command" "echo cfg-val")))
           ) ;list
         ) ;list
       ) ;calls
       (cfg '((provider . "google")))
       (res (pi-execute-tool-calls calls
              #f
              10
              (cons (cons 'execute-tool-call execute-tool-call) cfg)
            ) ;pi-execute-tool-calls
       ) ;res
      ) ;
  (check (length res) => 1)
  (let ((c (pi-tool-result-message-content (car res))))
    (check (pi-text-block-text (car c)) => "cfg-val\n")
  ) ;let
) ;let*

(check-report)
