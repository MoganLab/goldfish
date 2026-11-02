;; gemini-grounding-search-queries
;; 从提取出的 groundingDetails 或原始响应中读取搜索关键词列表。
;;
;; 语法
;; ----
;; (gemini-grounding-search-queries details)
;;
;; 参数
;; ----
;; details : alist
;; gemini-extract-grounding-details 的返回值，或可被其解析的原始响应。
;;
;; 返回值
;; ----
;; list
;; 搜索关键词字符串列表；未命中时返回空列表。

(import (scheme base)
  (liii check)
  (liii pi ai api google-generative-ai grounding-parse)
) ;import

(check-set-mode! 'report-failed)

;; 从已提取的 details 读取
(let ((details '((searchQueries "goldfish scheme" "goldfish github")
                 (searchResults . ())
                 (citations . ()))))
  (check (gemini-grounding-search-queries details) => '("goldfish scheme" "goldfish github"))
) ;let

;; 从原始 groundingMetadata alist 自动提取
(let ((raw '(("webSearchQueries" . #("scheme" "goldfish")))))
  (check (gemini-grounding-search-queries raw) => '("scheme" "goldfish"))
) ;let

;; 空 details 返回空列表
(check (gemini-grounding-search-queries '()) => '())
(check (gemini-grounding-search-queries #f) => '())

(check-report)
