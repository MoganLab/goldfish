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

;; reasoning 字段：thoughtsTokenCount 单列（output 的子集）
(let ((usage (gemini-map-usage '(("promptTokenCount" . 100)
                                 ("candidatesTokenCount" . 45)
                                 ("thoughtsTokenCount" . 5)))))
  (check (cdr (assoc 'reasoning usage)) => 5)
  (check (cdr (assoc 'output usage)) => 50)
) ;let*

;; 无 thinking 时 reasoning 为 0
(check (cdr (assoc 'reasoning (gemini-map-usage '(("promptTokenCount" . 10))))) => 0)

(check-report)
