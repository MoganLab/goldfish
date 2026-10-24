(import (liii check)
        (liii string)
        (liii pi ai message) (liii pi compact session) (liii pi session)
        (liii pi compact))

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

(check-report)
