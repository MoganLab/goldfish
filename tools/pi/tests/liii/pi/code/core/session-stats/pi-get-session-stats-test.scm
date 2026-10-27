(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii pi code core session-manager)
  (liii pi code core session-name)
  (liii pi code core session-stats)
) ;import

(check-set-mode! 'report-failed)

(define SESSION-FILE
  (path->string (path-join (path-temp-dir) "pi-session-stats-test.jsonl"))
) ;define

(when (file-exists? SESSION-FILE)
  (delete-file SESSION-FILE)
) ;when

(save-session-header! SESSION-FILE "session-stats-id" "2026-10-23T08:00:00.000Z"
  "/tmp/project"
) ;save-session-header!
(append-session-info! SESSION-FILE "Core stats")
(save-session-message! SESSION-FILE "{\"role\":\"user\",\"content\":\"hello\"}"
  "msg-user" #f "2026-10-23T08:00:01.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"working\"},{\"type\":\"toolCall\",\"id\":\"call-1\",\"name\":\"bash\",\"arguments\":{}},{\"type\":\"toolCall\",\"id\":\"call-2\",\"name\":\"read\",\"arguments\":{}}],\"usage\":{\"input\":1000,\"output\":200,\"cacheRead\":800,\"cacheWrite\":100,\"totalTokens\":2100,\"cost\":{\"total\":0.125}}}"
  "msg-assistant-1" "msg-user" "2026-10-23T08:00:02.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"toolResult\",\"toolCallId\":\"call-1\",\"toolName\":\"bash\",\"content\":[],\"isError\":false}"
  "msg-tool" "msg-assistant-1" "2026-10-23T08:00:03.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"done\"}],\"usage\":{\"input\":2000,\"output\":300,\"cacheRead\":400,\"cacheWrite\":0,\"totalTokens\":2700,\"cost\":{\"total\":0.375}}}"
  "msg-assistant-2" "msg-tool" "2026-10-23T08:00:04.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"bashExecution\",\"command\":\"pwd\",\"output\":\"/tmp/project\",\"exitCode\":0}"
  "msg-bash" "msg-assistant-2" "2026-10-23T08:00:05.000Z"
) ;save-session-message!

;; bashExecution 计入 Total，但不计入 User/Assistant/Tools
(let* ((stats (pi-get-session-stats SESSION-FILE))
       (tokens (pi-session-stats-tokens stats))
      ) ;
  (check (pi-session-stats-session-file stats) => SESSION-FILE)
  (check (pi-session-stats-session-id stats) => "session-stats-id")
  (check (pi-session-stats-session-name stats) => "Core stats")
  (check (pi-session-stats-total-messages stats) => 5)
  (check (pi-session-stats-user-messages stats) => 1)
  (check (pi-session-stats-assistant-messages stats) => 2)
  (check (pi-session-stats-tool-calls stats) => 2)
  (check (pi-session-stats-tool-results stats) => 1)
  (check (pi-session-token-stats-input tokens) => 3000)
  (check (pi-session-token-stats-output tokens) => 500)
  (check (pi-session-token-stats-cache-read tokens) => 1200)
  (check (pi-session-token-stats-cache-write tokens) => 100)
  (check (pi-session-token-stats-total tokens) => 4800)
  (check (pi-session-stats-cost stats) => 0.5)
) ;let*

(let ((empty-stats (pi-get-session-stats #f)))
  (check (pi-session-stats-session-file empty-stats) => #f)
  (check (pi-session-stats-total-messages empty-stats) => 0)
) ;let

(delete-file SESSION-FILE)

(check-report)
