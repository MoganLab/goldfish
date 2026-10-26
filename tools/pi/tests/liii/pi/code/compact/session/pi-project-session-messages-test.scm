(import (liii check)
        (liii string)
        (liii pi ai message) (liii pi code compact session) (liii pi code session)
        (liii pi code compact))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tests/fixtures/compact/sample-session.jsonl")
    "tests/fixtures/compact/sample-session.jsonl"
    "tools/pi/tests/fixtures/compact/sample-session.jsonl"
  ) ;if
) ;define

;; pi-read-session-entries: header + 消息 + compaction 全部条目
(let ((entries (pi-read-session-entries FIXTURE)))
  (check (length entries) => 6)
  (check (cdr (assoc "type" (car entries))) => "session")
  (check (cdr (assoc "id" (cadr entries))) => "u1")
) ;let

;; pi-project-session-messages: summary 首条 + firstKeptEntryId 起的原始消息
(let* ((entries (pi-read-session-entries FIXTURE))
       (msgs (pi-project-session-messages entries))
      ) ;
  (check (length msgs) => 3)
  (check (pi-message-role (car msgs)) => "user")
  (check-true (string-contains? (pi-user-message-content (car msgs))
                "fixture summary"
              ) ;string-contains?
  ) ;check-true
  (check (pi-message-role (cadr msgs)) => "user")
  (check (string=? (pi-user-message-content (cadr msgs)) "next question") => #t)
  (check (pi-message-role (caddr msgs)) => "assistant")
) ;let*

;; 无 compaction 条目时按顺序投影全部消息
(let* ((entries (list (list (cons "type" "session") (cons "id" "s1"))
                      (list (cons "type" "message")
                        (cons "id" "u1")
                        (cons "message" (list (cons "role" "user")
                                            (cons "content" "hello")
                                          ) ;list
                        ) ;cons
                      ) ;list
                    ) ;list
       ) ;entries
       (msgs (pi-project-session-messages entries))
      ) ;
  (check (length msgs) => 1)
  (check (pi-message-role (car msgs)) => "user")
) ;let*

;; bashExecution 条目投影为 user 消息（Ran + 围栏输出 + 退出码），
;; excludeFromContext 条目（!! 前缀）被跳过不进入 LLM 上下文
(let* ((entries (list (list (cons "type" "session") (cons "id" "s1"))
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
                      (list (cons "type" "message")
                        (cons "id" "b2")
                        (cons "message"
                          (list (cons "role" "bashExecution")
                            (cons "command" "secret")
                            (cons "output" "hidden")
                            (cons "exitCode" 0)
                            (cons "excludeFromContext" #t)
                          ) ;list
                        ) ;cons
                      ) ;list
                      (list (cons "type" "message")
                        (cons "id" "u1")
                        (cons "message" (list (cons "role" "user") (cons "content" "next")))
                      ) ;list
                    ) ;list
       ) ;entries
       (msgs (pi-project-session-messages entries))
      ) ;
  ;; 排除条目被过滤：只剩 bash 注入的 user 消息 + 普通 user 消息
  (check (length msgs) => 2)
  (check (pi-message-role (car msgs)) => "user")
  (check (pi-user-message-content (car msgs)) => "Ran `echo hi`\n```\nhi\n```")
  (check (pi-message-role (cadr msgs)) => "user")
  (check (pi-user-message-content (cadr msgs)) => "next")
) ;let*

;; 非零退出码的 bashExecution 条目投影文本附加退出码行
(let* ((entries (list (list (cons "type" "session") (cons "id" "s1"))
                      (list (cons "type" "message")
                        (cons "id" "b1")
                        (cons "message"
                          (list (cons "role" "bashExecution")
                            (cons "command" "false")
                            (cons "output" "boom")
                            (cons "exitCode" 1)
                          ) ;list
                        ) ;cons
                      ) ;list
                    ) ;list
       ) ;entries
       (msgs (pi-project-session-messages entries))
      ) ;
  (check (length msgs) => 1)
  (check (pi-user-message-content (car msgs))
    => "Ran `false`\n```\nboom\n```\n\nCommand exited with code 1")
) ;let*

(check-report)
