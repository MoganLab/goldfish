;; json->pi-event
;; 将 (liii json) 解析出的 Scheme-JSON 对象转换为事件 record。
;;
;; 语法
;; ----
;; (json->pi-event obj)
;;
;; 参数
;; ----
;; obj : any?
;; Scheme-JSON alist 或已有的 pi-event record。
;;
;; 返回值
;; ----
;; any?
;; 对应的 <pi-event-*> record 或 #f。

(import (liii check)
  (liii json)
  (liii pi ai event record)
  (liii pi ai event json)
  (liii pi ai message record)
  (liii pi ai tools record)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? json->pi-event))

;; 1. 幂等：已是 pi-event 直接返回
(let ((ev (pi-event-agent-start)))
  (check (json->pi-event ev) => ev)
) ;let

;; 2. session / agent_start / turn_start / agent_settled
(let* ((s (pi-event-session "/tmp/work" "id-1" "2026-01-01T00:00:00Z"))
       (rs (json->pi-event (string->json (json->string (pi-event->json s)))))
       (ra (json->pi-event (string->json (json->string (pi-event->json (pi-event-agent-start))))))
       (rt (json->pi-event (string->json (json->string (pi-event->json (pi-event-turn-start))))))
       (rset (json->pi-event (string->json (json->string (pi-event->json (pi-event-agent-settled #t))))))
      ) ;
  (check-true (pi-event-session? rs))
  (check (pi-event-session-id rs) => "id-1")
  (check (pi-event-session-cwd rs) => "/tmp/work")
  (check-true (pi-event-agent-start? ra))
  (check-true (pi-event-turn-start? rt))
  (check-true (pi-event-agent-settled? rset))
  (check (pi-event-agent-settled-aborted rset) => #t)
) ;let*

;; 3. message_end 嵌套 message 与 block
(let* ((msg (pi-assistant-message (list (pi-text-block "done")) :timestamp 123))
       (ev (pi-event-message-end msg))
       (r (json->pi-event (string->json (json->string (pi-event->json ev)))))
      ) ;
  (check-true (pi-event-message-end? r))
  (check-true (pi-assistant-message? (pi-event-message-end-message r)))
  (check-true
    (pi-text-block?
      (vector-ref (pi-assistant-message-content (pi-event-message-end-message r)) 0)
    ) ;pi-text-block?
  ) ;check-true
) ;let*

;; 4. tool_execution_end 嵌套 pi-tool-result
(let* ((res (pi-tool-result "ok" #f))
       (ev (pi-event-tool-execution-end "c1" "bash" res #f 42))
       (r (json->pi-event (string->json (json->string (pi-event->json ev)))))
      ) ;
  (check-true (pi-event-tool-execution-end? r))
  (check (pi-event-tool-execution-end-tool-call-id r) => "c1")
  (check (pi-event-tool-execution-end-duration-ms r) => 42)
  (check (pi-event-tool-execution-end-is-error r) => #f)
  (check-true (pi-tool-result? (pi-event-tool-execution-end-result r)))
) ;let*

;; 5. agent_end 嵌套消息列表
(let* ((msg (pi-user-message "hi" 100))
       (ev (pi-event-agent-end (list msg) #t '(("total" . 0.01))))
       (r (json->pi-event (string->json (json->string (pi-event->json ev)))))
      ) ;
  (check-true (pi-event-agent-end? r))
  (check (pi-event-agent-end-will-retry r) => #t)
  (check-true (pi-user-message? (car (pi-event-agent-end-messages r))))
  (check (json-ref (pi-event-agent-end-cost r) "total") => 0.01)
) ;let*

;; 6. 未知类型与非 pair 返回 #f
(check-false (json->pi-event '(("type" . "unknown_event"))))
(check-false (json->pi-event 123))

(check-report)
