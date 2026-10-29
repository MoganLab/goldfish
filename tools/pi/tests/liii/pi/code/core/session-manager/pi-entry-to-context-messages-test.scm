(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii pi ai message)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; ============================================================
;; 一、message 条目：按 role 分派，转换结果与现有投影逐字段一致
;; ============================================================

;; 1. user：原样转用户消息
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "u1")
           (cons "message" (list (cons "role" "user") (cons "content" "hello")))
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check-true (pi-user-message? (car msgs)))
  (check (pi-user-message-content (car msgs)) => "hello")
) ;let*

;; 2. assistant：保留 stopReason、errorMessage 与 usage
(let* ((usage (list (cons "inputTokens" 3) (cons "outputTokens" 5)))
       (entry
         (list (cons "type" "message")
           (cons "id" "a1")
           (cons "message"
             (list (cons "role" "assistant")
               (cons "content" (list (list (cons "type" "text") (cons "text" "hi"))))
               (cons "stopReason" "end_turn")
               (cons "errorMessage" #f)
               (cons "usage" usage)
             ) ;list
           ) ;cons
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
       (m (car msgs))
      ) ;
  (check (length msgs) => 1)
  (check-true (pi-assistant-message? m))
  (check (pi-assistant-message-stop-reason m) => "end_turn")
  (check (pi-assistant-message-error-message m) => #f)
  (check (pi-assistant-message-usage m) => usage)
  ;; content 中的块保持原始 alist（与旧投影 %as-pi-message 一致）
  (check (pi-assistant-message-content m)
    =>
    (list (list (cons "type" "text") (cons "text" "hi")))
  ) ;check
) ;let*

;; 3. assistant：字段缺失时 stopReason 兜底为 "stop"
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "a2")
           (cons "message"
             (list (cons "role" "assistant")
               (cons "content" (list (list (cons "type" "text") (cons "text" "hi"))))
             ) ;list
           ) ;cons
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
       (m (car msgs))
      ) ;
  (check-true (pi-assistant-message? m))
  (check (pi-assistant-message-stop-reason m) => "stop")
  (check (pi-assistant-message-error-message m) => #f)
  (check (pi-assistant-message-usage m) => #f)
) ;let*

;; 4. toolResult：保留 toolCallId、toolName 与 isError
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "t1")
           (cons "message"
             (list (cons "role" "toolResult")
               (cons "toolCallId" "call_1")
               (cons "toolName" "read")
               (cons "content" "file body")
               (cons "isError" #t)
             ) ;list
           ) ;cons
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
       (m (car msgs))
      ) ;
  (check (length msgs) => 1)
  (check-true (pi-tool-result-message? m))
  (check (pi-tool-result-message-tool-call-id m) => "call_1")
  (check (pi-tool-result-message-tool-name m) => "read")
  (check (pi-tool-result-message-content m) => "file body")
  (check (pi-tool-result-message-is-error m) => #t)
) ;let*

;; 5. toolResult：字段缺失时 toolCallId/toolName 兜底为空串、isError 兜底为 #f
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "t2")
           (cons "message" (list (cons "role" "toolResult")))
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
       (m (car msgs))
      ) ;
  (check (length msgs) => 1)
  (check-true (pi-tool-result-message? m))
  (check (pi-tool-result-message-tool-call-id m) => "")
  (check (pi-tool-result-message-tool-name m) => "")
  (check (pi-tool-result-message-is-error m) => #f)
) ;let*

;; ============================================================
;; 二、bashExecution：正常退出、非零退出码、excludeFromContext
;; ============================================================

;; 1. 正常退出（0）：Ran + 围栏输出，无退出码行
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "b1")
           (cons "message"
             (list (cons "role" "bashExecution")
               (cons "command" "echo hi")
               (cons "output" "hi")
               (cons "exitCode" 0)
             ) ;list
           ) ;cons
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check-true (pi-user-message? (car msgs)))
  (check (pi-user-message-content (car msgs)) => "Ran `echo hi`\n```\nhi\n```")
) ;let*

;; 2. 非零退出码：文本追加退出码行
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "b2")
           (cons "message"
             (list (cons "role" "bashExecution")
               (cons "command" "false")
               (cons "output" "boom")
               (cons "exitCode" 1)
             ) ;list
           ) ;cons
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check (pi-user-message-content (car msgs))
    =>
    "Ran `false`\n```\nboom\n```\n\nCommand exited with code 1"
  ) ;check
) ;let*

;; 3. excludeFromContext（!! 前缀）：不进入上下文，返回空列表
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "b3")
           (cons "message"
             (list (cons "role" "bashExecution")
               (cons "command" "secret")
               (cons "output" "hidden")
               (cons "exitCode" 0)
               (cons "excludeFromContext" #t)
             ) ;list
           ) ;cons
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check msgs => '())
) ;let*

;; 4. excludeFromContext 为 JSON true 符号（string->json 的 true 表示）
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "b4")
           (cons "message"
             (list (cons "role" "bashExecution")
               (cons "command" "secret")
               (cons "output" "hidden")
               (cons "exitCode" 0)
               (cons "excludeFromContext" 'true)
             ) ;list
           ) ;cons
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check msgs => '())
) ;let*

;; ============================================================
;; 三、content 兜底：缺失、为 null、为空块列表
;; ============================================================

;; 1. content 缺失
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "u2")
           (cons "message" (list (cons "role" "user")))
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check-true (pi-user-message? (car msgs)))
  (check (pi-user-message-content (car msgs)) => "")
) ;let*

;; 2. content 为 JSON null（string->json 解析为符号 null）
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "u3")
           (cons "message" (list (cons "role" "user") (cons "content" 'null)))
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check (pi-user-message-content (car msgs)) => "")
) ;let*

;; 3. content 为空块列表
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "a3")
           (cons "message" (list (cons "role" "assistant") (cons "content" '())))
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check-true (pi-assistant-message? (car msgs)))
  (check (pi-assistant-message-content (car msgs)) => '())
) ;let*

;; 4. assistant 的 content 缺失（含 stopReason 的旧会话条目）
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "a4")
           (cons "message" (list (cons "role" "assistant") (cons "stopReason" "stop")))
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check-true (pi-assistant-message? (car msgs)))
  (check (pi-assistant-message-content (car msgs)) => '())
  (check (pi-assistant-message-stop-reason (car msgs)) => "stop")
) ;let*

;; 5. toolResult 的 content 为 null
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "t3")
           (cons "message"
             (list (cons "role" "toolResult")
               (cons "toolCallId" "call_1")
               (cons "toolName" "read")
               (cons "content" 'null)
             ) ;list
           ) ;cons
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
       (m (car msgs))
      ) ;
  (check (length msgs) => 1)
  (check (pi-tool-result-message-content m) => '())
) ;let*

;; ============================================================
;; 四、compaction、session 与未知 type：返回空列表
;; ============================================================

;; 1. compaction 条目
(check (pi-entry-to-context-messages (list (cons "type" "compaction")
                                       (cons "id" "c1")
                                       (cons "summary" "s")
                                       (cons "firstKeptEntryId" "u1")
                                     ) ;list
       ) ;pi-entry-to-context-messages
  =>
  '()
) ;check

;; 2. session header 条目
(check (pi-entry-to-context-messages (list (cons "type" "session")
                                       (cons "version" 3)
                                       (cons "id" "s1")
                                       (cons "cwd" "/tmp")
                                     ) ;list
       ) ;pi-entry-to-context-messages
  =>
  '()
) ;check

;; 3. 未知 type
(check (pi-entry-to-context-messages (list (cons "type" "custom_message") (cons "id" "x1"))
       ) ;pi-entry-to-context-messages
  =>
  '()
) ;check

;; 4. type 缺失
(check (pi-entry-to-context-messages (list (cons "id" "x2"))) => '())

;; ============================================================
;; 五、兼容符号键（会话内构造的条目可能用符号键）
;; ============================================================

(let* ((entry
         (list (cons 'type "message")
           (cons 'id "u9")
           (cons 'message (list (cons 'role "user") (cons 'content "sym key")))
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check (pi-user-message-content (car msgs)) => "sym key")
) ;let*

(check-report)
