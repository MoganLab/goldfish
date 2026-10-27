(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(check (gemini-build-thinking-config "off") => #f)
(check (gemini-build-thinking-config #f) => #f)

(let ((cfg (gemini-build-thinking-config "low")))
  (check-true (pair? cfg))
  (let ((tc (cdr (assoc "thinkingConfig" cfg))))
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "LOW")
  ) ;let
) ;let

(let ((cfg (gemini-build-thinking-config "high")))
  (check-true (pair? cfg))
  (let ((tc (cdr (assoc "thinkingConfig" cfg))))
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "HIGH")
  ) ;let
) ;let

(check-report)
