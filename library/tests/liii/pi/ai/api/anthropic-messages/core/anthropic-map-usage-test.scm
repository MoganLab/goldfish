;; anthropic-map-usage
;; 将 Anthropic 响应的 usage 统计映射为内部统一的 usage alist。
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

(import (liii check)
  (liii pi ai api anthropic-messages core)
  (liii pi ai event)
) ;import

(check-set-mode! 'report-failed)

;; 完整 usage 字段映射
(let* ((raw-usage '(("input_tokens" . 100)
                    ("output_tokens" . 45)
                    ("cache_read_input_tokens" . 20)
                    ("cache_creation_input_tokens" . 10)))
       (usage (anthropic-map-usage raw-usage)))
  (check (cdr (assoc 'input usage)) => 100)
  (check (cdr (assoc 'output usage)) => 45)
  (check (cdr (assoc 'cacheRead usage)) => 20)
  (check (cdr (assoc 'cacheWrite usage)) => 10)
  (check (cdr (assoc 'totalTokens usage)) => 175)
) ;let*

;; 缺省字段补零
(let ((usage (anthropic-map-usage '(("input_tokens" . 7)))))
  (check (cdr (assoc 'input usage)) => 7)
  (check (cdr (assoc 'output usage)) => 0)
  (check (cdr (assoc 'cacheRead usage)) => 0)
  (check (cdr (assoc 'cacheWrite usage)) => 0)
  (check (cdr (assoc 'totalTokens usage)) => 7)
) ;let*

;; 空 usage
(let ((usage (anthropic-map-usage '())))
  (check (cdr (assoc 'input usage)) => 0)
  (check (cdr (assoc 'totalTokens usage)) => 0)
) ;let*

(check-report)

;; reasoning 字段：output_tokens_details.thinking_tokens 单列（output 的子集）
(let ((usage (anthropic-map-usage '(("input_tokens" . 100)
                                    ("output_tokens" . 45)
                                    ("output_tokens_details" . (("thinking_tokens" . 12)))))))
  (check (cdr (assoc 'reasoning usage)) => 12)
  (check (cdr (assoc 'output usage)) => 45)
) ;let

;; 无 thinking details 时 reasoning 为 0
(check (cdr (assoc 'reasoning (anthropic-map-usage '(("input_tokens" . 7))))) => 0)

(check-report)
