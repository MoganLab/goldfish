;; anthropic-map-usage
;; 映射 Anthropic 响应 usage 统计数据。
;;
;; 语法
;; ----
;; (anthropic-map-usage usage-meta)
;;
;; 参数
;; ----
;; usage-meta : alist
;; Anthropic 返回的 usage 对象，包含 input_tokens、output_tokens、
;; cache_read_input_tokens 与 cache_creation_input_tokens 等字段。
;;
;; 返回值
;; ----
;; alist
;; 内部 usage alist，包含 input、output、cacheRead、cacheWrite 与 totalTokens。

(import (liii check) (liii pi ai api anthropic-messages) (liii pi ai event))

(check-set-mode! 'report-failed)

(let* ((raw-usage '(("input_tokens" . 100)
                    ("output_tokens" . 45)
                    ("cache_read_input_tokens" . 20)
                    ("cache_creation_input_tokens" . 10)))
       (usage (anthropic-map-usage raw-usage)))
  (check (cdr (assoc 'input usage)) => 100)
  (check (cdr (assoc 'output usage)) => 45)
  (check (cdr (assoc 'cacheRead usage)) => 20)
  (check (cdr (assoc 'cacheWrite usage)) => 10)
  (check (cdr (assoc 'totalTokens usage)) => 145))

(check-report)
