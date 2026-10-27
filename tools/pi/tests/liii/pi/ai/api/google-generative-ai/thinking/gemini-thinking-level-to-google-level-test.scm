(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(check (gemini-thinking-level->google-level "minimal") => "MINIMAL")
(check (gemini-thinking-level->google-level "low") => "LOW")
(check (gemini-thinking-level->google-level "medium") => "MEDIUM")
(check (gemini-thinking-level->google-level "high") => "HIGH")
(check (gemini-thinking-level->google-level "xhigh") => "HIGH")
(check (gemini-thinking-level->google-level "max") => "HIGH")
(check (gemini-thinking-level->google-level "off") => #f)
(check (gemini-thinking-level->google-level "unknown") => #f)

(check-report)
