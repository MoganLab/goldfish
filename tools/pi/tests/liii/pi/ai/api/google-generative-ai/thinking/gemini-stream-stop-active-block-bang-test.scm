(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (gemini-append-thinking-block! stream-state "Thinking 1" "sig-1" emit)
  (gemini-append-thinking-block! stream-state " and more" #f emit)

  (gemini-stream-stop-active-block! stream-state emit)
  (check (vector-ref stream-state 5) => #f)
  (check (length events) => 4)
  (check (cdr (assoc 'type (list-ref events 3))) => "thinking_end")
  (check (cdr (assoc 'content (list-ref events 3))) => "Thinking 1 and more")
) ;let*

(check-report)
