(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

(check (gemini-build-endpoint #f "gemini-2.0-flash")
  =>
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"
) ;check

(check (gemini-build-endpoint "https://generativelanguage.googleapis.com/v1beta/"
         "gemini-2.0-flash"
       ) ;gemini-build-endpoint
  =>
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"
) ;check

(check (gemini-build-endpoint "https://proxy.example.com/v1" "gemini-2.0-flash")
  =>
  "https://proxy.example.com/v1/models/gemini-2.0-flash:generateContent"
) ;check

(check-report)
