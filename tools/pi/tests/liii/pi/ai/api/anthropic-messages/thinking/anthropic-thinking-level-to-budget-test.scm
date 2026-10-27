(import (liii check) (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

(check (anthropic-thinking-level->budget "minimal") => 1024)
(check (anthropic-thinking-level->budget "low") => 2048)
(check (anthropic-thinking-level->budget "medium") => 8192)
(check (anthropic-thinking-level->budget "high") => 16384)
(check (anthropic-thinking-level->budget "xhigh") => 16384)
(check (anthropic-thinking-level->budget "max") => 16384)
(check (anthropic-thinking-level->budget "off") => #f)
(check (anthropic-thinking-level->budget "unknown") => #f)

(check-report)
