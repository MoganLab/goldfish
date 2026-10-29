(import (liii check)
  (liii string)
  (liii pi ai message)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; ============================================================
;; pi-entry-to-context-messages：会话条目 -> LLM 上下文消息（0 或 1 条）
;; ============================================================

;; 1. user 条目：content 原样转为用户消息
(let* ((entry
         (list (cons "type" "message")
           (cons "id" "e1")
           (cons "message" (list (cons "role" "user") (cons "content" "hello")))
         ) ;list
       ) ;entry
       (msgs (pi-entry-to-context-messages entry))
      ) ;
  (check (length msgs) => 1)
  (check (pi-message-role (car msgs)) => "user")
  (check (pi-user-message-content (car msgs)) => "hello")
) ;let*

;; 2. assistant 条目：保留 stopReason、errorMessage 与 usage
(let* ((usage (list (cons "input" 10) (cons "output" 5)))
       (msg-alist
         (list (cons "role" "assistant")
           (cons "content" (list (list (cons "type" "text") (cons "text" "hi"))))
           (cons "stopReason" "end_turn")
           (cons "errorMessage" #f)
           (cons "usage" usage)
         ) ;list
       ) ;msg-alist
       (msgs (pi-entry-to-context-messages (list (cons "type" "message") (cons "id" "e2") (cons "message" msg-alist))
             ) ;pi-entry-to-context-messages
       ) ;msgs
      ) ;
  (check (length msgs) => 1)
  (check (pi-message-role (car msgs)) => "assistant")
  (check (pi-assistant-message-stop-reason (car msgs)) => "end_turn")
  (check-false (pi-assistant-message-error-message (car msgs)))
  (check (pi-assistant-message-usage (car msgs)) => usage)
) ;let*

;; 3. toolResult 条目：保留 toolCallId、toolName 与 isError
(let* ((content (list (list (cons "type" "text") (cons "text" "ok"))))
       (msg-alist (list (cons "role" "toolResult")
                    (cons "toolCallId" "call_1")
                    (cons "toolName" "bash")
                    (cons "content" content)
                    (cons "isError" #f)
                  ) ;list
       ) ;msg-alist
       (msgs (pi-entry-to-context-messages (list (cons "type" "message") (cons "id" "e3") (cons "message" msg-alist))
             ) ;pi-entry-to-context-messages
       ) ;msgs
      ) ;
  (check (length msgs) => 1)
  (check (pi-message-role (car msgs)) => "toolResult")
  (check (pi-tool-result-message-tool-call-id (car msgs)) => "call_1")
  (check (pi-tool-result-message-tool-name (car msgs)) => "bash")
  (check-false (pi-tool-result-message-is-error (car msgs)))
) ;let*

;; 4. bashExecution 正常退出：转为 user 文本（命令 + 围栏输出）
(let* ((msg-alist (list (cons "role" "bashExecution")
                    (cons "command" "echo hi")
                    (cons "output" "hi")
                    (cons "exitCode" 0)
                  ) ;list
       ) ;msg-alist
       (msgs (pi-entry-to-context-messages (list (cons "type" "message") (cons "id" "e4") (cons "message" msg-alist))
             ) ;pi-entry-to-context-messages
       ) ;msgs
      ) ;
  (check (length msgs) => 1)
  (check (pi-message-role (car msgs)) => "user")
  (check (pi-user-message-content (car msgs)) => "Ran `echo hi`\n```\nhi\n```")
) ;let*

;; 5. bashExecution 非零退出码：文本追加退出码行
(let* ((msg-alist (list (cons "role" "bashExecution")
                    (cons "command" "false")
                    (cons "output" "boom")
                    (cons "exitCode" 1)
                  ) ;list
       ) ;msg-alist
       (msgs (pi-entry-to-context-messages (list (cons "type" "message") (cons "id" "e5") (cons "message" msg-alist))
             ) ;pi-entry-to-context-messages
       ) ;msgs
      ) ;
  (check (length msgs) => 1)
  (check (pi-user-message-content (car msgs))
    =>
    "Ran `false`\n```\nboom\n```\n\nCommand exited with code 1"
  ) ;check
) ;let*

;; 6. bashExecution 带 excludeFromContext（!! 前缀）：返回空列表
(let* ((msg-alist (list (cons "role" "bashExecution")
                    (cons "command" "secret-cmd")
                    (cons "output" "hidden")
                    (cons "exitCode" 0)
                    (cons "excludeFromContext" 'true)
                  ) ;list
       ) ;msg-alist
       (msgs (pi-entry-to-context-messages (list (cons "type" "message") (cons "id" "e6") (cons "message" msg-alist))
             ) ;pi-entry-to-context-messages
       ) ;msgs
      ) ;
  (check msgs => '())
) ;let*

;; 7. content 缺失：user/assistant/toolResult 均降级为空内容
(let* ((u
         (pi-entry-to-context-messages
           (list (cons "type" "message")
             (cons "id" "e7")
             (cons "message" (list (cons "role" "user")))
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;u
       (a
         (pi-entry-to-context-messages
           (list (cons "type" "message")
             (cons "id" "e8")
             (cons "message" (list (cons "role" "assistant")))
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;a
       (t
         (pi-entry-to-context-messages
           (list (cons "type" "message")
             (cons "id" "e9")
             (cons "message" (list (cons "role" "toolResult") (cons "toolCallId" "c1")))
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;t
      ) ;
  (check (pi-user-message-content (car u)) => "")
  (check (pi-assistant-message-content (car a)) => '())
  (check (pi-tool-result-message-content (car t)) => '())
) ;let*

;; 8. content 为 null（JSON null）：同缺失，降级为空内容
(let* ((u
         (pi-entry-to-context-messages
           (list (cons "type" "message")
             (cons "id" "e10")
             (cons "message" (list (cons "role" "user") (cons "content" 'null)))
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;u
       (a
         (pi-entry-to-context-messages
           (list (cons "type" "message")
             (cons "id" "e11")
             (cons "message" (list (cons "role" "assistant") (cons "content" 'null)))
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;a
       (t
         (pi-entry-to-context-messages
           (list (cons "type" "message")
             (cons "id" "e12")
             (cons "message"
               (list (cons "role" "toolResult")
                 (cons "toolCallId" "c1")
                 (cons "content" 'null)
               ) ;list
             ) ;cons
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;t
      ) ;
  (check (pi-user-message-content (car u)) => "")
  (check (pi-assistant-message-content (car a)) => '())
  (check (pi-tool-result-message-content (car t)) => '())
) ;let*

;; 9. content 为空块列表（[] / 空列表）：原样保留，不抛异常
(let* ((a-empty
         (pi-entry-to-context-messages
           (list (cons "type" "message")
             (cons "id" "e13")
             (cons "message" (list (cons "role" "assistant") (cons "content" '())))
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;a-empty
       (t-empty
         (pi-entry-to-context-messages
           (list (cons "type" "message")
             (cons "id" "e14")
             (cons "message"
               (list (cons "role" "toolResult") (cons "toolCallId" "c1") (cons "content" '()))
             ) ;cons
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;t-empty
      ) ;
  (check (pi-assistant-message-content (car a-empty)) => '())
  (check (pi-tool-result-message-content (car t-empty)) => '())
) ;let*

;; 10. compaction / session / 未知 type：返回空列表
(check (pi-entry-to-context-messages (list (cons "type" "compaction") (cons "id" "c") (cons "summary" "s"))
       ) ;pi-entry-to-context-messages
  =>
  '()
) ;check
(check (pi-entry-to-context-messages (list (cons "type" "session") (cons "id" "s1") (cons "cwd" "/tmp"))
       ) ;pi-entry-to-context-messages
  =>
  '()
) ;check
(check (pi-entry-to-context-messages (list (cons "type" "something-else") (cons "id" "x"))
       ) ;pi-entry-to-context-messages
  =>
  '()
) ;check

;; 11. 兼容符号键（同为符号键的会话条目）
(let* ((msgs
         (pi-entry-to-context-messages
           (list (cons 'type "message")
             (cons 'id 'sym1)
             (cons 'message (list (cons 'role "user") (cons 'content "sym hello")))
           ) ;list
         ) ;pi-entry-to-context-messages
       ) ;msgs
      ) ;
  (check (length msgs) => 1)
  (check (pi-user-message-content (car msgs)) => "sym hello")
) ;let*

(check-report)
