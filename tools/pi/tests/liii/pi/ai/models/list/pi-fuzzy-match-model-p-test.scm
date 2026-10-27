(import (liii check)
        (liii pi ai models list))

(check-set-mode! 'report-failed)

;; pi-fuzzy-match-model?
;; 判断搜索词是否与模型文本进行子序列模糊匹配。
;;
;; 语法
;; ----
;; (pi-fuzzy-match-model? search-query text)
;;
;; 参数
;; ----
;; search-query : string | #f
;; 以空白或 / 分隔的搜索词。
;; text : string
;; 待匹配的 provider 和模型文本。
;;
;; 返回值
;; ----
;; boolean
;; 所有 token 都按序作为子序列出现时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 匹配忽略大小写；空 query 或非字符串 query 默认全匹配。
;;
;; 示例
;; ----
;; (pi-fuzzy-match-model? "google/flash" "google gemini-2.5-flash") => #t

(check-true (pi-fuzzy-match-model? "g25f" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "flash" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "google flash" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "google/flash" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "sonnet" "anthropic claude-sonnet-5"))

;; 负例
(check-false (pi-fuzzy-match-model? "anthropic flash" "google gemini-2.5-flash"))
(check-false (pi-fuzzy-match-model? "g25f" "google gemini-3.8-flash"))
(check-false (pi-fuzzy-match-model? "xyz" "google gemini-2.5-flash"))

;; 空 query 或非字符串 query 默认全匹配，大小写不影响匹配
(check-true (pi-fuzzy-match-model? "" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? #f "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? 42 "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "GEMINI FLASH" "google gemini-2.5-flash"))

(check-report)
