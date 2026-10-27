(import (liii check) (liii pi code core compaction))

(check-set-mode! 'report-failed)

(define (message-entry id role text)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "message" (list (cons "role" role) (cons "content" text)))
  ) ;list
) ;define

;; user/assistant 可切，toolResult 不可切
(check-true (pi-valid-cut-point? (message-entry "u1" "user" "hi")))
(check-true (pi-valid-cut-point? (message-entry "a1" "assistant" "hi")))
(check-false (pi-valid-cut-point? (message-entry "t1" "toolResult" "hi")))
(check-false (pi-valid-cut-point? (list (cons "type" "compaction") (cons "id" "c1")))
) ;check-false

(define entries
  (list (message-entry "u1" "user" (make-string 40 #\a))
    (message-entry "a1" "assistant" (make-string 40 #\a))
    (message-entry "u2" "user" (make-string 40 #\a))
    (message-entry "a2" "assistant" (make-string 40 #\a))
  ) ;list
) ;define

(check (map pi-valid-cut-point? entries) => '(#t #t #t #t))

(check-report)
