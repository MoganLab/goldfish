(import (liii check)
        (liii pi blueprint parse)
) ;import

(check-set-mode! 'report-failed)

;; pi-extract-blueprint-json
(check (pi-extract-blueprint-json "{\"a\":1}") => "{\"a\":1}")

;; 前后有说明文字时只取 JSON 对象。
(check (pi-extract-blueprint-json "Here is the plan:\n{\"a\":1}\nThanks") => "{\"a\":1}")

;; Markdown 围栏。
(check (pi-extract-blueprint-json "```json\n{\"a\":1}\n```") => "{\"a\":1}")

;; 没有 JSON 对象时返回 #f。
(check (pi-extract-blueprint-json "no json here") => #f)
(check (pi-extract-blueprint-json "") => #f)

(check-report)
