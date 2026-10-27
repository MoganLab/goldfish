(import (liii check) (liii pi code core compaction))

(check-set-mode! 'report-failed)

(define (message-entry id role text)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "message" (list (cons "role" role) (cons "content" text)))
  ) ;list
) ;define

(define entries
  (list (message-entry "u1" "user" (make-string 40 #\a))
    (message-entry "a1" "assistant" (make-string 40 #\a))
    (message-entry "u2" "user" (make-string 40 #\a))
    (message-entry "a2" "assistant" (make-string 40 #\a))
  ) ;list
) ;define

(check (length (pi-messages-to-summarize entries 4)) => 4)

;; 只收集切点之前的 message 条目
(let ((msgs (pi-messages-to-summarize entries 2)))
  (check (length msgs) => 2)
  (check (cdr (assoc "role" (car msgs))) => "user")
) ;let

(check-report)
