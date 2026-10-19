(import (liii check)
        (liii pi models list))

(check-set-mode! 'report-failed)

;; pi-split-search-tokens
;; 按空白或 / 切分搜索词为 token 列表

(check (pi-split-search-tokens "gemini flash") => '("gemini" "flash"))
(check (pi-split-search-tokens "google/gemini") => '("google" "gemini"))
(check (pi-split-search-tokens "  g25f  ") => '("g25f"))
(check (pi-split-search-tokens "google / gemini-2.5 ") => '("google" "gemini-2.5"))
(check (pi-split-search-tokens "") => '())
(check (pi-split-search-tokens #f) => '())

(check-report)
