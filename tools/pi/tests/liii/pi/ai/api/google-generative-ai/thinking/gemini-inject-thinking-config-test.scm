(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(let* ((base '(("contents" . #())))
       (injected (gemini-inject-thinking-config base "low"))
      ) ;
  (check-true (pair? (assoc "generationConfig" injected)))
  (let* ((gc (cdr (assoc "generationConfig" injected)))
         (tc (cdr (assoc "thinkingConfig" gc)))
        ) ;
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "LOW")
  ) ;let*
) ;let*

(let* ((base '(("contents" . #())))
       (injected (gemini-inject-thinking-config base "off"))
      ) ;
  (check-false (assoc "generationConfig" injected))
  (check injected => base)
) ;let*

(check-report)
