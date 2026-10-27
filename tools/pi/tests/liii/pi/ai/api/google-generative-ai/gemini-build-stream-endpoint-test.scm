(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

(check (gemini-build-stream-endpoint #f "gemini-2.0-flash")
  =>
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse"
) ;check

(check (gemini-build-stream-endpoint "https://generativelanguage.googleapis.com/v1beta/"
         "gemini-2.0-flash"
       ) ;gemini-build-stream-endpoint
  =>
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse"
) ;check

(check-report)
