;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii pi)
) ;import

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
(let* ((sess (pi-event-session "/path/to/cwd" "test-session-id" "2026-10-17T08:00:00.000Z"))
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
(let* ((u-msg '(("role" . "user") ("content" . "hello") ("timestamp" . 1733234401000)))
       (ev-start (pi-event-message-start u-msg))
       (ev-end (pi-event-message-end u-msg))
      ) ;
  (check (cdr (assoc "type" ev-start)) => "message_start")
  (check (cdr (assoc "type" ev-end)) => "message_end")
  (check-true (string-contains? (pi-serialize-event ev-start) "\"role\":\"user\""))
) ;let*

;; 4. tool execution start & end
(let* ((tool-start (pi-event-tool-execution-start "call_123" "bash" '(("command" . "ls"))))
       (tool-end (pi-event-tool-execution-end "call_123" "bash" '(("content" . "file.txt")) #f))
      ) ;
  (check (cdr (assoc "type" tool-start)) => "tool_execution_start")
  (check (cdr (assoc "toolCallId" tool-start)) => "call_123")
  (check (cdr (assoc "toolName" tool-start)) => "bash")
  (check (cdr (assoc "type" tool-end)) => "tool_execution_end")
  (check (cdr (assoc "isError" tool-end)) => #f)
) ;let*

;; 5. turn_end, agent_end, agent_settled
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
