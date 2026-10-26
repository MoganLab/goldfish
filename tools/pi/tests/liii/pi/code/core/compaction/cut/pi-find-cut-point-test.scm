(import (liii check)
        (liii pi code core compaction))

(check-set-mode! 'report-failed)

(define (message-entry id role text)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "message" (list (cons "role" role) (cons "content" text)))
  ) ;list
) ;define

(define (tool-entry id role text)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "message" (list (cons "role" role) (cons "content" text)))
  ) ;list
) ;define

;; pi-valid-cut-point?: user/assistant 可切，toolResult 不可切
(check-true (pi-valid-cut-point? (message-entry "u1" "user" "hi")))
(check-true (pi-valid-cut-point? (message-entry "a1" "assistant" "hi")))
(check-false (pi-valid-cut-point? (tool-entry "t1" "toolResult" "hi")))
(check-false (pi-valid-cut-point? (list (cons "type" "compaction")
                                    (cons "id" "c1")
                                  ) ;list
             ) ;list
) ;check-false

;; 4 条消息，每条 40 字符 -> 10 tokens
(define entries
  (list (message-entry "u1" "user" (make-string 40 #\a))
    (message-entry "a1" "assistant" (make-string 40 #\a))
    (message-entry "u2" "user" (make-string 40 #\a))
    (message-entry "a2" "assistant" (make-string 40 #\a))
  ) ;list
) ;define

;; keepRecentTokens=20 -> 自最新向前累加到第二条即停，切点 index 2
(check (pi-estimate-message-tokens (pi-entry-message (car entries))) => 10)
(check (map pi-valid-cut-point? entries) => '(#t #t #t #t))
(check (length (pi-messages-to-summarize entries 4)) => 4)
(let ((cp (pi-find-cut-point entries 20)))
  (check (cdr (assoc 'first-kept-index cp)) => 2)
  (check (cdr (assoc 'is-split-turn cp)) => #f)
) ;let

;; keepRecentTokens=5 -> 只保留最后一条
(let ((cp (pi-find-cut-point entries 5)))
  (check (cdr (assoc 'first-kept-index cp)) => 3)
) ;let

;; 预算大于全部消息 -> 默认取第一个合法切点
(let ((cp (pi-find-cut-point entries 1000)))
  (check (cdr (assoc 'first-kept-index cp)) => 0)
) ;let

;; toolResult 不成为切点
(let ((mixed (list (message-entry "u1" "user" (make-string 40 #\a))
                   (tool-entry "t1" "toolResult" (make-string 40 #\a))
                   (message-entry "a1" "assistant" (make-string 40 #\a))
                 ) ;list
           ) ;mixed
      ) ;
  (check (cdr (assoc 'first-kept-index (pi-find-cut-point mixed 5))) => 2)
) ;let

;; pi-messages-to-summarize 只收集切点之前的 message 条目
(let ((msgs (pi-messages-to-summarize entries 2)))
  (check (length msgs) => 2)
  (check (cdr (assoc "role" (car msgs))) => "user")
) ;let

(check-report)
