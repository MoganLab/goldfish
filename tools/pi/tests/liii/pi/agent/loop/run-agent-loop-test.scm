;; run-agent-loop
;; 驱动 Agent 对话主循环：执行多轮模型请求与工具调用，直至自然停止或达到限制。
;;
;; 语法
;; ----
;; (run-agent-loop config initial-messages system-prompt (emit #f) (timeout 600) (max-turns 0) (session-timeout 0) (compaction #f) (request-fn pi-request-provider-stream) (emit-initial? #t))
;;
;; 参数
;; ----
;; config : list?，包含 tools、provider 以及工具执行器的 Agent 配置 alist。
;; initial-messages : list?，初始消息历史列表。
;; system-prompt : (or string? procedure?)，系统提示词模板或回调函数。
;; emit : (or procedure? #f)，事件派发回调过程。
;; timeout : real?，单轮超时时间（秒）。
;; max-turns : integer?，最大执行轮数（0 表示无限制）。
;; session-timeout : real?，会话超时时间（0 表示无限制）。
;; compaction : (or list? #f)，上下文压缩配置。
;; request-fn : (or procedure? #f)，模型请求发送函数。
;; emit-initial? : boolean?，是否派发初始消息的上屏事件。
;;
;; 返回值
;; ----
;; list?，执行完成后的完整消息历史列表。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi ai tools record)
  (liii pi agent loop)
) ;import

(check-set-mode! 'report-failed)

;; 1. 单轮纯文本交互：模型直接回复并自然结束
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (mock-req
         (lambda (provider cfg msgs sp tools on-stream to)
           (pi-assistant-message (list (list (cons 'type "text") (cons 'text "done!")))
             :stop-reason "stop"
             :usage       '((input . 1) (output . 1) (total . 2))
           ) ;pi-assistant-message
         ) ;lambda
       ) ;mock-req
       (cfg '((tools)
              (provider . "google")
              (model-id . "gemini-3.8-flash")
              (models-store))
       ) ;cfg
       (res (run-agent-loop cfg (list (pi-user-message "hi")) "" emit 10 0 0 #f mock-req)
       ) ;res
      ) ;
  (check (length res) => 2)
  (check-true (pi-assistant-message? (cadr res)))
  (check-true (pair? (member 'agent_start evs)))
  (check-true (pair? (member 'turn_start evs)))
  (check-true (pair? (member 'turn_end evs)))
  (check-true (pair? (member 'agent_end evs)))
) ;let*

;; 2. 两轮交互：第 1 轮调用工具，第 2 轮结束
(let* ((round 0)
       (mock-req
         (lambda (provider cfg msgs sp tools on-stream to)
           (set! round (+ round 1))
           (if (= round 1)
             (pi-assistant-message (list (list (cons 'type "toolCall")
                                           (cons 'id "call_1")
                                           (cons 'name "dummy")
                                           (cons 'arguments ())
                                         ) ;list
                                   ) ;list
               :stop-reason "toolUse"
               :usage       '((input . 1) (output . 1) (total . 2))
             ) ;pi-assistant-message
             (pi-assistant-message (list (list (cons 'type "text") (cons 'text "all done")))
               :stop-reason "stop"
               :usage       '((input . 1) (output . 1) (total . 2))
             ) ;pi-assistant-message
           ) ;if
         ) ;lambda
       ) ;mock-req
       (mock-exec (lambda (name args . opts) (pi-tool-result "dummy output" #f)))
       (cfg (list (cons 'tools '())
              (cons 'execute-tool-call mock-exec)
              (cons 'provider "google")
              (cons 'model-id "gemini-3.8-flash")
              (cons 'models-store '())
            ) ;list
       ) ;cfg
       (res (run-agent-loop cfg '() "" #f 10 0 0 #f mock-req))
      ) ;
  ;; 消息历史包含：第1轮assistant、第1轮toolResult、第2轮assistant，共3条
  (check (length res) => 3)
  (check-true (pi-tool-result-message? (cadr res)))
) ;let*

(check-report)
