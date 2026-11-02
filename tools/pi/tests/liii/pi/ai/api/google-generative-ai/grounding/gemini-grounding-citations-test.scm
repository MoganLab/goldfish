;; gemini-grounding-citations
;; 从提取出的 groundingDetails 或原始响应中读取引用片段列表。
;;
;; 语法
;; ----
;; (gemini-grounding-citations details)
;;
;; 参数
;; ----
;; details : alist
;; gemini-extract-grounding-details 的返回值，或可被其解析的原始响应。
;;
;; 返回值
;; ----
;; list
;; 引用 alist 列表，每项包含 title、url、citedText、startIndex、endIndex 等字段。

(import (scheme base)
  (liii check)
  (liii pi ai api google-generative-ai grounding)
) ;import

(check-set-mode! 'report-failed)

;; 从已提取的 details 读取
(let* ((item '((title . "A") (url . "https://a.com") (citedText . "hello")))
       (details (list (cons 'searchQueries '())
                      (cons 'searchResults '())
                      (cons 'citations (list item)))))
  (check (length (gemini-grounding-citations details)) => 1)
  (check (cdr (assoc 'citedText (car (gemini-grounding-citations details)))) => "hello")
) ;let*

;; 空 details 返回空列表
(check (gemini-grounding-citations '()) => '())
(check (gemini-grounding-citations #f) => '())

(check-report)
