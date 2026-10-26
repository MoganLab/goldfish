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

(save-session-header! SESSION-FILE "session-stats-id" "2026-10-23T08:00:00.000Z" "/tmp/project")
(append-session-info! SESSION-FILE "Core stats")
(save-session-message! SESSION-FILE
  "{\"role\":\"user\",\"content\":\"hello\"}"
  "msg-user"
  #f
  "2026-10-23T08:00:01.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"working\"},{\"type\":\"toolCall\",\"id\":\"call-1\",\"name\":\"bash\",\"arguments\":{}},{\"type\":\"toolCall\",\"id\":\"call-2\",\"name\":\"read\",\"arguments\":{}}],\"usage\":{\"input\":1000,\"output\":200,\"cacheRead\":800,\"cacheWrite\":100,\"totalTokens\":2100,\"cost\":{\"total\":0.125}}}"
  "msg-assistant-1"
  "msg-user"
  "2026-10-23T08:00:02.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"toolResult\",\"toolCallId\":\"call-1\",\"toolName\":\"bash\",\"content\":[],\"isError\":false}"
  "msg-tool"
  "msg-assistant-1"
  "2026-10-23T08:00:03.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"done\"}],\"usage\":{\"input\":2000,\"output\":300,\"cacheRead\":400,\"cacheWrite\":0,\"totalTokens\":2700,\"cost\":{\"total\":0.375}}}"
  "msg-assistant-2"
  "msg-tool"
  "2026-10-23T08:00:04.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"bashExecution\",\"command\":\"pwd\",\"output\":\"/tmp/project\",\"exitCode\":0}"
  "msg-bash"
  "msg-assistant-2"
  "2026-10-23T08:00:05.000Z"
) ;save-session-message!

;; /session 聚合所有消息 entry；bashExecution 计入 Total，但不计入 User/Assistant/Tools。
(let* ((stats (pi-get-session-stats SESSION-FILE))
       (tokens (cdr (assoc 'tokens stats))))
  (check (cdr (assoc 'session-file stats)) => SESSION-FILE)
  (check (cdr (assoc 'session-id stats)) => "session-stats-id")
  (check (cdr (assoc 'session-name stats)) => "Core stats")
  (check (cdr (assoc 'total-messages stats)) => 5)
  (check (cdr (assoc 'user-messages stats)) => 1)
  (check (cdr (assoc 'assistant-messages stats)) => 2)
  (check (cdr (assoc 'tool-calls stats)) => 2)
  (check (cdr (assoc 'tool-results stats)) => 1)
  (check (cdr (assoc 'input tokens)) => 3000)
  (check (cdr (assoc 'output tokens)) => 500)
  (check (cdr (assoc 'cache-read tokens)) => 1200)
  (check (cdr (assoc 'cache-write tokens)) => 100)
  (check (cdr (assoc 'total tokens)) => 4800)
  (check (cdr (assoc 'cost stats)) => 0.5)
) ;let*

;; 文本布局对齐上游核心区块；不包含当前尚不支持的 Cache Warming、分模型费用等。
(let ((text (pi-format-session-stats (pi-get-session-stats SESSION-FILE))))
  (check text =>
    (string-append
      "Session Info\n\n"
      "Name: Core stats\n"
      "File: " SESSION-FILE "\n"
      "ID: session-stats-id\n\n"
      "Messages\n"
      "Total: 5\n"
      "User: 1\n"
      "Assistant: 2\n"
      "Tools: 2 calls, 1 results\n\n"
      "Tokens\n"
      "Input: 4,300\n"
      "  Cached: 1,200 (27.9%)\n"
      "  Uncached: 3,100 (100 written to cache)\n"
      "Output: 500\n"
      "Total: 4,800\n\n"
      "Cost\n"
      "Total: $0.500"
    ) ;string-append
  ) ;check
) ;let

;; 没有名称且费用为零时省略对应可选区块。
(let ((empty-stats (pi-get-session-stats #f)))
  (check (cdr (assoc 'session-file empty-stats)) => #f)
  (check (cdr (assoc 'total-messages empty-stats)) => 0)
  (check (pi-format-session-stats empty-stats) =>
    "Session Info\n\nFile: In-memory\nID: Unavailable\n\nMessages\nTotal: 0\nUser: 0\nAssistant: 0\nTools: 0 calls, 0 results\n\nTokens\nInput: 0\nOutput: 0\nTotal: 0"
  ) ;check
) ;let

(delete-file SESSION-FILE)

(check-report)
