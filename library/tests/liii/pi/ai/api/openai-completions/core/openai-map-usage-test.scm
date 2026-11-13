;; openai-map-usage
;; 将各大 OpenAI 兼容供应商（OpenAI、DeepSeek、Moonshot、智谱等）响应 usage
;; 映射为内部标准 usage alist。
;;
;; 语法
;; ----
;; (openai-map-usage usage-meta)
;;
;; 参数
;; ----
;; usage-meta : alist
;; 服务商返回的 usage 对象，可能包含 prompt_tokens、completion_tokens、total_tokens、
;; prompt_tokens_details（cached_tokens、cache_write_tokens）、prompt_cache_hit_tokens
;; 以及顶层 cached_tokens 等字段。
;;
;; 返回值
;; ----
;; alist
;; 内部标准 usage alist，包含 input、output、cacheRead、cacheWrite 与 totalTokens。
;; 其中 input 为扣减 cacheRead 与 cacheWrite 后的真实未缓存输入 token 数。

(import (liii check) (liii pi ai event) (liii pi ai api openai-completions) (liii pi ai message))

(check-set-mode! 'report-failed)

;; 1. 标准 OpenAI / 智谱嵌套格式 (prompt_tokens_details.cached_tokens)
(let* ((raw '(("prompt_tokens" . 120)
              ("completion_tokens" . 45)
              ("total_tokens" . 165)
              ("prompt_tokens_details" . (("cached_tokens" . 80)
                                          ("cache_write_tokens" . 0)))))
       (u (openai-map-usage raw)))
  (check (cdr (assoc 'input u)) => 40)
  (check (cdr (assoc 'output u)) => 45)
  (check (cdr (assoc 'cacheRead u)) => 80)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 165))

;; 2. DeepSeek 风格格式 (prompt_cache_hit_tokens)
(let* ((raw '(("prompt_tokens" . 200)
              ("completion_tokens" . 50)
              ("total_tokens" . 250)
              ("prompt_cache_hit_tokens" . 150)
              ("prompt_cache_miss_tokens" . 50)))
       (u (openai-map-usage raw)))
  (check (cdr (assoc 'input u)) => 50)
  (check (cdr (assoc 'output u)) => 50)
  (check (cdr (assoc 'cacheRead u)) => 150)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 250))

;; 3. Moonshot / Kimi 顶层 cached_tokens 格式
(let* ((raw '(("prompt_tokens" . 100)
              ("completion_tokens" . 30)
              ("total_tokens" . 130)
              ("cached_tokens" . 60)))
       (u (openai-map-usage raw)))
  (check (cdr (assoc 'input u)) => 40)
  (check (cdr (assoc 'output u)) => 30)
  (check (cdr (assoc 'cacheRead u)) => 60)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 130))

;; 4. 同时包含 cacheRead 与 cacheWrite (prompt_tokens_details)
(let* ((raw '(("prompt_tokens" . 150)
              ("completion_tokens" . 50)
              ("total_tokens" . 200)
              ("prompt_tokens_details" . (("cached_tokens" . 80)
                                          ("cache_write_tokens" . 20)))))
       (u (openai-map-usage raw)))
  (check (cdr (assoc 'input u)) => 50)
  (check (cdr (assoc 'output u)) => 50)
  (check (cdr (assoc 'cacheRead u)) => 80)
  (check (cdr (assoc 'cacheWrite u)) => 20)
  (check (cdr (assoc 'totalTokens u)) => 200))

;; 5. 无任何缓存活动（首轮普通调用，或符号 key）
(let* ((raw '((prompt_tokens . 100)
              (completion_tokens . 25)
              (total_tokens . 125)))
       (u (openai-map-usage raw)))
  (check (cdr (assoc 'input u)) => 100)
  (check (cdr (assoc 'output u)) => 25)
  (check (cdr (assoc 'cacheRead u)) => 0)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 125))

;; 6. 空 usage 边界防御
(let ((u (openai-map-usage '())))
  (check (cdr (assoc 'input u)) => 0)
  (check (cdr (assoc 'output u)) => 0)
  (check (cdr (assoc 'cacheRead u)) => 0)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 0))

(check-report)

;; 7. reasoning 字段：completion_tokens_details.reasoning_tokens 单列
(let* ((raw '(("prompt_tokens" . 100)
              ("completion_tokens" . 50)
              ("completion_tokens_details" . (("reasoning_tokens" . 30)))))
       (u (openai-map-usage raw)))
  (check (cdr (assoc 'reasoning u)) => 30)
  (check (cdr (assoc 'output u)) => 50)
) ;let*

;; 8. 无 reasoning details 时 reasoning 为 0
(let ((u (openai-map-usage '(("prompt_tokens" . 10) ("completion_tokens" . 5)))))
  (check (cdr (assoc 'reasoning u)) => 0)
) ;let

;; 9. JSON null（符号 'null）字段防御
(let* ((raw '(("prompt_tokens" . 100)
              ("completion_tokens" . 20)
              ("prompt_tokens_details" . null)
              ("completion_tokens_details" . null)
              ("cached_tokens" . null)
              ("prompt_cache_hit_tokens" . null)))
       (u (openai-map-usage raw)))
  (check (cdr (assoc 'input u)) => 100)
  (check (cdr (assoc 'output u)) => 20)
  (check (cdr (assoc 'cacheRead u)) => 0)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'reasoning u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 120)
) ;let*

(check-report)
