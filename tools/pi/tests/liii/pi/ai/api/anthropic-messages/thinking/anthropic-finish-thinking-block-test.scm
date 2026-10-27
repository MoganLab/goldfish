(import (liii check)
  (liii pi ai message)
  (liii pi ai api anthropic-messages thinking)
) ;import

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
  (anthropic-handle-thinking-delta '(("type" . "signature_delta")
                                     ("signature" . "sig123"))
    0
    stream-state
    emit
  ) ;anthropic-handle-thinking-delta

  (anthropic-finish-thinking-block stream-state emit)
  (check (length events) => 2)
  (check (cdr (assoc 'type (cadr events))) => "thinking_end")
  (check (cdr (assoc 'content (cadr events))) => "Step 1")
  (check (length (vector-ref stream-state 0)) => 1)
  (let ((blk (car (vector-ref stream-state 0))))
    (check-true (pi-thinking-block? blk))
    (check (pi-thinking-block-thinking blk) => "Step 1")
    (check (pi-thinking-block-thinking-signature blk) => "sig123")
  ) ;let
) ;let*

(check-report)
