(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (gemini-append-thinking-block! stream-state "Thinking 1" "sig-1" emit)
  (check (vector-ref stream-state 5) => "thinking")
  (check (vector-ref stream-state 6) => "Thinking 1")
  (check (length (vector-ref stream-state 0)) => 1)
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "thinking_start")
  (check (cdr (assoc 'type (cadr events))) => "thinking_delta")

  (gemini-append-thinking-block! stream-state " and more" #f emit)
  (check (vector-ref stream-state 6) => "Thinking 1 and more")
  (check (length (vector-ref stream-state 0)) => 1)
  (check (length events) => 3)
  (check (cdr (assoc 'type (caddr events))) => "thinking_delta")
) ;let*

(check-report)
