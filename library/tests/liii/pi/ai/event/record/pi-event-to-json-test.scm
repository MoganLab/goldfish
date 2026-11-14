;; pi-event->json
;; 将事件 record 转换为符合 (liii json) 规范的 Scheme-JSON 对象。
;;
;; 语法
;; ----
;; (pi-event->json ev)
;;
;; 参数
;; ----
;; ev : any?
;; <pi-event-*> record 或 alist。
;;
;; 返回值
;; ----
;; any?
;; 可直接用于 json->string 或 json-ref 的 Scheme-JSON 对象。

(import (liii check)
  (liii json)
  (liii pi ai event record)
  (liii pi ai message record)
  (liii pi ai tools record)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-event->json))

;; 1. session 事件
(let ((j (pi-event->json (pi-event-session "/tmp/work" "id-1" "2026-01-01T00:00:00Z"))))
  (check (json-ref j "type") => "session")
  (check (json-ref j "version") => 3)
  (check (json-ref j "id") => "id-1")
  (check (json-ref j "cwd") => "/tmp/work")
  (check (string->json (json->string j)) => j)
) ;let

;; 2. message_end 嵌套 pi-assistant-message
(let* ((msg (pi-assistant-message (list (pi-text-block "done")) :timestamp 123))
       (j (pi-event->json (pi-event-message-end msg)))
      ) ;
  (check (json-ref j "type") => "message_end")
  (check (json-ref j "message" "role") => "assistant")
  (check (json-ref (vector-ref (json-ref j "message" "content") 0) "text") => "done")
  (check-false (json-contains-key? (json-ref j "message") "errorMessage"))
) ;let*

;; 3. tool_execution_end 嵌套 pi-tool-result
(let* ((res (pi-tool-result "ok" #f))
       (j (pi-event->json (pi-event-tool-execution-end "c1" "bash" res #f 42)))
      ) ;
  (check (json-ref j "type") => "tool_execution_end")
  (check (json-ref j "toolCallId") => "c1")
  (check (json-ref j "durationMs") => 42)
  (check (json-ref (vector-ref (json-ref j "result" "content") 0) "text") => "ok")
) ;let*

;; 4. agent_end 嵌套消息列表
(let* ((msg (pi-user-message "hi" 100))
       (j (pi-event->json (pi-event-agent-end (list msg) #f '((total . 0.01)))))
      ) ;
  (check (json-ref j "type") => "agent_end")
  (check-true (vector? (json-ref j "messages")))
  (check (json-ref (vector-ref (json-ref j "messages") 0) "role") => "user")
  (check (json-ref j "cost" "total") => 0.01)
) ;let*

(check-report)
