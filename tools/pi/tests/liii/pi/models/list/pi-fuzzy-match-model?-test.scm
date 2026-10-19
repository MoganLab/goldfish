(import (liii check)
        (liii pi models list))

(check-set-mode! 'report-failed)

;; pi-fuzzy-match-model?
;; 子序列模糊匹配：所有 query token 都必须作为子序列按序出现在 text 中

(check-true (pi-fuzzy-match-model? "g25f" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "flash" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "google flash" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "google/flash" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "sonnet" "anthropic claude-sonnet-5"))

;; 负例
(check-false (pi-fuzzy-match-model? "anthropic flash" "google gemini-2.5-flash"))
(check-false (pi-fuzzy-match-model? "g25f" "google gemini-3.8-flash"))
(check-false (pi-fuzzy-match-model? "xyz" "google gemini-2.5-flash"))

;; 空 query 或 #f 默认全匹配
(check-true (pi-fuzzy-match-model? "" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? #f "google gemini-2.5-flash"))

(check-report)
