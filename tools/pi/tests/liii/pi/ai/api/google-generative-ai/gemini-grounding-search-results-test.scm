;; gemini-grounding-search-results
;; 从提取出的 groundingDetails 或原始响应中读取网页搜索结果列表。
;;
;; 语法
;; ----
;; (gemini-grounding-search-results details)
;;
;; 参数
;; ----
;; details : alist
;; gemini-extract-grounding-details 的返回值，或可被其解析的原始响应。
;;
;; 返回值
;; ----
;; list
;; 搜索结果 alist 列表，每项包含 title、url、source、type 与 raw。

(import (scheme base)
  (liii check)
  (liii pi ai api google-generative-ai)
) ;import

(check-set-mode! 'report-failed)

;; 从已提取的 details 读取
(let* ((item '((title . "Goldfish") (url . "https://goldfish.scheme.org")))
       (details (list (cons 'searchQueries '())
                      (cons 'searchResults (list item))
                      (cons 'citations '()))))
  (check (length (gemini-grounding-search-results details)) => 1)
  (check (cdr (assoc 'title (car (gemini-grounding-search-results details)))) => "Goldfish")
) ;let*

;; 空 details 返回空列表
(check (gemini-grounding-search-results '()) => '())
(check (gemini-grounding-search-results #f) => '())

(check-report)
