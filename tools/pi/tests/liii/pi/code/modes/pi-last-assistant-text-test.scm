(import (liii check)
  (liii pi code modes output))

(check-set-mode! 'report-failed)

(define (text-message text)
  (list (cons 'content (vector (list (cons 'type "text") (cons 'text text))))))

(check (pi-last-assistant-text '()) => "")
(check (pi-last-assistant-text (list (text-message "first") (text-message "last"))) => "last")
(check (pi-last-assistant-text (list (text-message "last") (text-message "  \n"))) => "last")

(check-report)
