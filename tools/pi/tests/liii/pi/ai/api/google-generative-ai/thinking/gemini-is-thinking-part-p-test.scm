(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(check-true (gemini-is-thinking-part? '(("text" . "thinking...")
                                        ("thought" . #t)))
) ;check-true
(check-true (gemini-is-thinking-part? '((thought . #t) (text . "thinking..."))))
(check-false (gemini-is-thinking-part? '(("text" . "regular answer"))))
(check-false (gemini-is-thinking-part? '(("text" . "answer") ("thought" . #f))))

(check-report)
