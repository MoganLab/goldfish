(import (liii check) (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (vector-set! stream-state 6 '((thinking . "") (signature . "") (index . 0)))

  (anthropic-handle-thinking-delta '(("type" . "thinking_delta")
                                     ("thinking" . "Step 1"))
    0
    stream-state
    emit
  ) ;anthropic-handle-thinking-delta
  (check (length events) => 1)
  (check (cdr (assoc 'type (car events))) => "thinking_delta")
  (check (cdr (assoc 'delta (car events))) => "Step 1")

  (anthropic-handle-thinking-delta '(("type" . "signature_delta")
                                     ("signature" . "sig123"))
    0
    stream-state
    emit
  ) ;anthropic-handle-thinking-delta
  (check (length events) => 1)
) ;let*

(check-report)
