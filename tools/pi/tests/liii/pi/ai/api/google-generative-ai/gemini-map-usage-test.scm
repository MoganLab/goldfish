;; gemini-map-usage
;; 映射 Gemini usageMetadata 到 pi agent 的 usage 格式。
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

(import (liii check) (liii pi ai event) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

(let* ((raw-u '(("promptTokenCount" . 100) ("candidatesTokenCount" . 50)
                ("thoughtsTokenCount" . 20) ("cachedContentTokenCount" . 30)
                ("totalTokenCount" . 170)))
       (u (gemini-map-usage raw-u)))
  (check (cdr (assoc 'input u)) => 70)
  (check (cdr (assoc 'output u)) => 70)
  (check (cdr (assoc 'cacheRead u)) => 30)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 170))

(check-report)
