;; anthropic-inject-thinking
;; 按思考级别向 Anthropic 请求体注入 thinking 配置。
;;
;; 语法
;; ----
;; (anthropic-inject-thinking req-body level)
;;
;; 参数
;; ----
;; req-body : alist
;; Anthropic 请求体，可包含 max_tokens 等字段。
;; level : string
;; off、minimal、low、medium、high、xhigh 或 max。
;;
;; 返回值
;; ----
;; alist
;; 注入 thinking 配置后的请求体；level 为 off 时返回原请求体。
;;
;; 说明
;; ----
;; 按请求体中的 "model" 选择格式：
;; - 自适应思考模型（见 anthropic-adaptive-thinking-model?）：注入
;;   thinking = {"type": "adaptive", "display": "summarized"} 与
;;   output_config = {"effort": ...}；max_tokens 不变。Opus / Sonnet 4.6 没有
;;   xhigh 档，降为 high。
;; - 其他模型：注入 thinking = {"type": "enabled", "budget_tokens": N}；
;;   max_tokens 小于或等于预算时提升为 budget_tokens + 4096。
;; 其他字段及其相对顺序保持不变。

(import (liii check) (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

;; off 时不注入
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "off"))
      ) ;
  (check-false (assoc "thinking" res))
) ;let*

;; low 时注入 thinking 并保持 max_tokens (8192 > 2048)
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "low"))
      ) ;
  (check-true (pair? (assoc "thinking" res)))
  (let ((th (cdr (assoc "thinking" res))))
    (check (cdr (assoc "type" th)) => "enabled")
    (check (cdr (assoc "budget_tokens" th)) => 2048)
  ) ;let
  (check (cdr (assoc "max_tokens" res)) => 8192)
) ;let*

;; high 时自动提升过小的 max_tokens
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "high"))
      ) ;
  (check (cdr (assoc "max_tokens" res)) => (+ 16384 4096))
) ;let*

;; 提升 max_tokens 时保持其他字段及 entry 顺序
(let* ((body '(("model" . "claude-3-7-sonnet")
               ("max_tokens" . 1024)
               ("temperature" . 0))
       ) ;body
       (res (anthropic-inject-thinking body "low"))
      ) ;
  (check (map car res) => '("thinking" "model" "max_tokens" "temperature"))
  (check (cdr (assoc "max_tokens" res)) => (+ 2048 4096))
  (check (cdr (assoc "temperature" res)) => 0)
) ;let*

;; 自适应思考模型：adaptive + effort，不改 max_tokens
(let* ((body '(("model" . "claude-opus-5-5") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "high"))
       (th (cdr (assoc "thinking" res)))
       (oc (cdr (assoc "output_config" res)))
      ) ;
  (check (cdr (assoc "type" th)) => "adaptive")
  (check (cdr (assoc "display" th)) => "summarized")
  (check-false (assoc "budget_tokens" th))
  (check (cdr (assoc "effort" oc)) => "high")
  (check (cdr (assoc "max_tokens" res)) => 8192)
  (check (map car res) => '("thinking" "output_config" "model" "max_tokens"))
) ;let*

;; 自适应思考模型：minimal 映射为 low
(let* ((body '(("model" . "claude-haiku-5-5") ("max_tokens" . 1024)))
       (res (anthropic-inject-thinking body "minimal"))
      ) ;
  (check
    (cdr (assoc "effort" (cdr (assoc "output_config" res))))
    =>
    "low"
  ) ;check
  (check (cdr (assoc "max_tokens" res)) => 1024)
) ;let*

;; 自适应思考模型：off 时不注入 thinking 与 output_config
(let* ((body '(("model" . "claude-sonnet-5-5") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "off"))
      ) ;
  (check res => body)
) ;let*

;; Opus 4.6 没有 xhigh 档，降为 high；Opus 4.8 保留 xhigh
(let ((effort-of
        (lambda (model)
          (let ((res (anthropic-inject-thinking (list (cons "model" model) (cons "max_tokens" 8192))
                       "xhigh"
                     ) ;anthropic-inject-thinking
                ) ;res
               ) ;
            (cdr (assoc "effort" (cdr (assoc "output_config" res))))
          ) ;let
        ) ;lambda
      ) ;effort-of
     ) ;
  (check (effort-of "claude-opus-4-6") => "high")
  (check (effort-of "claude-opus-4-8") => "xhigh")
) ;let

;; Anthropic 兼容端点的非 Claude 模型保持 budget_tokens
(let* ((body '(("model" . "deepseek-v4-pro") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "medium"))
       (th (cdr (assoc "thinking" res)))
      ) ;
  (check (cdr (assoc "type" th)) => "enabled")
  (check (cdr (assoc "budget_tokens" th)) => 8192)
  (check-false (assoc "output_config" res))
) ;let*

(check-report)
