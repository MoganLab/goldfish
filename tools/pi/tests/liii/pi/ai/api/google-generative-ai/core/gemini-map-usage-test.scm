;; gemini-map-usage
;; 将 Gemini 响应的 usageMetadata 映射为内部统一的 usage alist。
;;
;; 语法
;; ----
;; (gemini-map-usage usage-meta)
;;
;; 参数
;; ----
;; usage-meta : alist
;; Gemini 返回的 usageMetadata 对象，包含 promptTokenCount、
;; candidatesTokenCount、thoughtsTokenCount、cachedContentTokenCount
;; 与 totalTokenCount 等字段。
;;
;; 返回值
;; ----
;; alist
;; 内部 usage alist，包含 input、output、cacheRead、cacheWrite 与 totalTokens。

(import (liii check)
  (liii pi ai api google-generative-ai core)
) ;import

(check-set-mode! 'report-failed)

;; 完整 usage 字段映射
(let ((usage (gemini-map-usage '(("promptTokenCount" . 100)
                                 ("candidatesTokenCount" . 45)
                                 ("thoughtsTokenCount" . 5)
                                 ("cachedContentTokenCount" . 20)
                                 ("totalTokenCount" . 150)))))
  (check (cdr (assoc 'input usage)) => 80)
  (check (cdr (assoc 'output usage)) => 50)
  (check (cdr (assoc 'cacheRead usage)) => 20)
  (check (cdr (assoc 'cacheWrite usage)) => 0)
  (check (cdr (assoc 'totalTokens usage)) => 150)
) ;let*

;; 缺省字段补零
(let ((usage (gemini-map-usage '(("promptTokenCount" . 7)))))
  (check (cdr (assoc 'input usage)) => 7)
  (check (cdr (assoc 'output usage)) => 0)
  (check (cdr (assoc 'cacheRead usage)) => 0)
  (check (cdr (assoc 'totalTokens usage)) => 0)
) ;let*

;; 空 usage
(let ((usage (gemini-map-usage '())))
  (check (cdr (assoc 'input usage)) => 0)
  (check (cdr (assoc 'output usage)) => 0)
  (check (cdr (assoc 'totalTokens usage)) => 0)
) ;let*

(check-report)
