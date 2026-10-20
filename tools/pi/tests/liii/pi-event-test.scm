(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (liii check) (liii string) (liii pi))

(check-set-mode! 'report-failed)

;; ISO 8601 与当前毫秒时间戳
(let ((iso-now (pi-iso-8601-utc-now)))
  (check-true (string? iso-now))
  (check-true (string-ends? iso-now "Z"))
  (check-true (string-contains? iso-now "T"))
) ;let

(let ((ms (pi-current-time-millis)))
  (check-true (number? ms))
  (check-true (> ms 1700000000000))
) ;let

;; 1. session header 事件
(let* ((sess (pi-event-session "/path/to/cwd" "test-session-id" "2026-10-17T08:00:00.000Z")
       ) ;sess
       (json-str (pi-serialize-event sess))
      ) ;
  (check (cdr (assoc "type" sess)) => "session")
  (check (cdr (assoc "version" sess)) => 3)
  (check (cdr (assoc "id" sess)) => "test-session-id")
  (check (cdr (assoc "timestamp" sess)) => "2026-10-17T08:00:00.000Z")
  (check (cdr (assoc "cwd" sess)) => "/path/to/cwd")
  (check-true (string-contains? json-str "\"type\":\"session\""))
  (check-true (string-contains? json-str "\"version\":3"))
) ;let*

;; 2. agent_start & turn_start
(check (cdr (assoc "type" (pi-event-agent-start))) => "agent_start")
(check (cdr (assoc "type" (pi-event-turn-start))) => "turn_start")

;; 3. message_start & message_end
(let* ((u-msg '(("role" . "user")
                ("content" . "hello")
                ("timestamp" . 1733234401000)))
       (ev-start (pi-event-message-start u-msg))
       (ev-end (pi-event-message-end u-msg))
      ) ;
  (check (cdr (assoc "type" ev-start)) => "message_start")
  (check (cdr (assoc "type" ev-end)) => "message_end")
  (check-true (string-contains? (pi-serialize-event ev-start) "\"role\":\"user\"")
  ) ;check-true
) ;let*

;; 4. tool execution start & end
(let* ((tool-start (pi-event-tool-execution-start "call_123" "bash" '(("command"
                                                                       . "ls")))
       ) ;tool-start
       (tool-end (pi-event-tool-execution-end "call_123" "bash" '(("content"
                                                                   . "file.txt")) #f)
       ) ;tool-end
      ) ;
  (check (cdr (assoc "type" tool-start)) => "tool_execution_start")
  (check (cdr (assoc "toolCallId" tool-start)) => "call_123")
  (check (cdr (assoc "toolName" tool-start)) => "bash")
  (check (cdr (assoc "type" tool-end)) => "tool_execution_end")
  (check (cdr (assoc "isError" tool-end)) => #f)
) ;let*

;; 5. message_update (阶段七：流式事件)
;; 5.1 text_delta
(let* ((usage '(("input" . 10) ("output" . 5) ("total" . 15)))
       (delta-ev '(("type" . "text_delta")
                   ("contentIndex" . 0)
                   ("delta" . "Hello ")))
       (upd (pi-event-message-update usage delta-ev))
      ) ;
  (check (cdr (assoc "type" upd)) => "message_update")
  (check
    (cdr (assoc "input" (cdr (assoc "usage" upd))))
    =>
    10
  ) ;check
  (let ((a-ev (cdr (assoc "assistantMessageEvent" upd))))
    (check (cdr (assoc "type" a-ev)) => "text_delta")
    (check (cdr (assoc "contentIndex" a-ev)) => 0)
    (check (cdr (assoc "delta" a-ev)) => "Hello ")
  ) ;let
  ;; 验证序列化
  (let ((serialized (pi-serialize-event upd)))
    (check-true (string? serialized))
    (check-true (string-contains serialized "\"type\":\"message_update\""))
    (check-true (string-contains serialized "\"delta\":\"Hello \""))
  ) ;let
) ;let*

;; 5.2 toolcall_start (需要带 id 与 toolName)
(let* ((usage '(("input" . 10) ("output" . 5) ("total" . 15)))
       (tc-ev '(("type" . "toolcall_start")
                ("contentIndex" . 1)
                ("id" . "call_1")
                ("toolName" . "bash"))
       ) ;tc-ev
       (upd (pi-event-message-update usage tc-ev))
      ) ;
  (check (cdr (assoc "type" upd)) => "message_update")
  (let ((a-ev (cdr (assoc "assistantMessageEvent" upd))))
    (check (cdr (assoc "type" a-ev)) => "toolcall_start")
    (check (cdr (assoc "id" a-ev)) => "call_1")
    (check (cdr (assoc "toolName" a-ev)) => "bash")
  ) ;let
) ;let*

;; 6. tool_execution_update
(let* ((args '(("command" . "ls")))
       (partial-res
         '(("content" (("type" . "text") ("text" . "partial"))))
       ) ;partial-res
       (ev (pi-event-tool-execution-update "call_1" "bash" args partial-res))
      ) ;
  (check (cdr (assoc "type" ev)) => "tool_execution_update")
  (check (cdr (assoc "toolCallId" ev)) => "call_1")
  (check (cdr (assoc "toolName" ev)) => "bash")
  (check
    (cdr (assoc "command" (cdr (assoc "args" ev))))
    =>
    "ls"
  ) ;check
  (let ((serialized (pi-serialize-event ev)))
    (check-true (string-contains serialized "\"type\":\"tool_execution_update\""))
  ) ;let
) ;let*

;; 7. turn_end, agent_end, agent_settled
(let* ((a-msg '(("role" . "assistant")))
       (ev-turn-end (pi-event-turn-end a-msg #()))
       (ev-agent-end (pi-event-agent-end (list a-msg) #f))
       (ev-settled (pi-event-agent-settled #f))
      ) ;
  (check (cdr (assoc "type" ev-turn-end)) => "turn_end")
  (check (cdr (assoc "type" ev-agent-end)) => "agent_end")
  (check (cdr (assoc "willRetry" ev-agent-end)) => #f)
  (check (cdr (assoc "type" ev-settled)) => "agent_settled")
  (check (cdr (assoc "aborted" ev-settled)) => #f)
) ;let*

(check-report)
