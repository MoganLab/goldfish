;; pi-handle-tool-use
;; 执行工具调用、追加工具执行结果消息，并派发当前轮次结束事件通知。
;;
;; 语法
;; ----
;; (pi-handle-tool-use new-msgs tool-calls emit timeout config resp)
;;
;; 参数
;; ----
;; new-msgs : list?，当前追加了 assistant 消息的列表。
;; tool-calls : list?，待执行的工具调用块列表。
;; emit : (or procedure? #f)，事件派发过程。
;; timeout : real?，工具执行超时时间（秒）。
;; config : list?，包含工具执行器的 Agent 配置 alist。
;; resp : any?，本轮 assistant 响应对象。
;;
;; 返回值
;; ----
;; (values list? boolean?)，返回追加了工具执行结果的全部消息列表与是否终止的布尔值。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi ai tools record)
  (liii pi agent loop)
) ;import

(check-set-mode! 'report-failed)

;; 1. 执行工具调用并触发 turn_end 事件，返回所有消息及 term? 为 #f
(let* ((evs '())
       (emit
         (lambda (ev-type . args) (set! evs (cons (cons ev-type args) evs)))
       ) ;emit
       (mock-exec (lambda (name args . opts) (pi-tool-result "tool output" #f)))
       (cfg (list (cons 'execute-tool-call mock-exec)))
       (resp (pi-assistant-message '() :stop-reason "toolUse"))
       (new-msgs (list resp))
       (calls (list '((id . "c1") (name . "bash") (arguments))))
      ) ;
  (let-values (((all-msgs terminated?) (pi-handle-tool-use new-msgs calls emit 10 cfg resp)))
    (check (length all-msgs) => 2)
    (check-true (pi-tool-result-message? (cadr all-msgs)))
    (check terminated? => #f)
    (check-true (pair? (assoc 'turn_end evs)))
  ) ;let-values
) ;let*

;; 2. emit 为 #f 正常返回
(let* ((mock-exec (lambda (name args . opts) (pi-tool-result "tool output" #f)))
       (cfg (list (cons 'execute-tool-call mock-exec)))
       (resp (pi-assistant-message '() :stop-reason "toolUse"))
       (calls (list '((id . "c1") (name . "bash") (arguments))))
      ) ;
  (let-values (((all-msgs terminated?) (pi-handle-tool-use (list resp) calls #f 10 cfg resp)))
    (check (length all-msgs) => 2)
    (check terminated? => #f)
  ) ;let-values
) ;let*

(check-report)
