(import (liii check) (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

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
;; max_tokens 小于或等于预算时提升为 budget_tokens + 4096。
;; 其他字段及其相对顺序保持不变。
;;
;; 示例
;; ----
;; (anthropic-inject-thinking '(("max_tokens" . 1024)) "low") => (("thinking" ...) ("max_tokens" . 6144))

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
               ("temperature" . 0)))
       (res (anthropic-inject-thinking body "low")))
  (check (map car res) => '("thinking" "model" "max_tokens" "temperature"))
  (check (cdr (assoc "max_tokens" res)) => (+ 2048 4096))
  (check (cdr (assoc "temperature" res)) => 0))

(check-report)
