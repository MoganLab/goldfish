(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii pi code core session-manager)
  (liii pi code core session-name)
  (liii pi code core session-stats)
) ;import

(check-set-mode! 'report-failed)

;; pi-format-session-stats
;; 将 pi-get-session-stats 的统计 alist 格式化为可读的纯文本报告。
;;
;; 语法
;; ----
;; (pi-format-session-stats stats) -> string
;;
;; 参数
;; ----
;; stats : alist
;; 包含会话名称、消息数量、token 用量和费用的统计信息。
;;
;; 返回值
;; ----
;; string
;; 多行文本报告；大整数按三位分组，费用保留三位小数。
;; 没有缓存、名称或费用信息时省略相应附加内容。
;;
;; 示例
;; ----
;; (pi-format-session-stats (pi-get-session-stats "session.jsonl"))

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

;; 不包含当前尚不支持的 Cache Warming、分模型费用等
(let ((text (pi-format-session-stats (pi-get-session-stats SESSION-FILE))))
  (check text
    =>
    (string-append "Session Info\n\n" "Name: Core stats\n" "File: " SESSION-FILE
      "\n" "ID: session-stats-id\n\n" "Messages\n" "Total: 5\n" "User: 1\n"
      "Assistant: 2\n" "Tools: 2 calls, 1 results\n\n" "Tokens\n"
      "Input: 4,300\n" "  Cached: 1,200 (27.9%)\n"
      "  Uncached: 3,100 (100 written to cache)\n" "Output: 500\n"
      "Total: 4,800\n\n" "Cost\n" "Total: $0.500"
    ) ;string-append
  ) ;check
) ;let

;; 没有名称且费用为零时省略对应可选区块
(let ((empty-stats (pi-get-session-stats #f)))
  (check (pi-format-session-stats empty-stats)
    =>
    "Session Info\n\nFile: In-memory\nID: Unavailable\n\nMessages\nTotal: 0\nUser: 0\nAssistant: 0\nTools: 0 calls, 0 results\n\nTokens\nInput: 0\nOutput: 0\nTotal: 0"
  ) ;check
) ;let

(delete-file SESSION-FILE)

(check-report)
