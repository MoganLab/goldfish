;; anthropic-adaptive-thinking-model?
;; 判断模型是否使用自适应思考（thinking.type = "adaptive" + output_config.effort）。
;;
;; 语法
;; ----
;; (anthropic-adaptive-thinking-model? model-id)
;;
;; 参数
;; ----
;; model-id : string
;; 模型 id，允许带 "anthropic/" 之类的前缀与日期后缀。
;;
;; 返回值
;; ----
;; boolean
;; Fable / Mythos 全系、Opus / Sonnet 4.6 及以后、Haiku 5 及以后返回 #t；
;; 其余 Claude 模型、非 Claude 模型与非字符串返回 #f。
;;
;; 说明
;; ----
;; 这些模型对 {"type": "enabled", "budget_tokens": N} 返回 HTTP 400，
;; 需要改用 {"type": "adaptive"} 并通过 output_config.effort 控制思考深度。

(import (liii check) (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

;; 当前模型
(check-true (anthropic-adaptive-thinking-model? "claude-opus-5-5"))
(check-true (anthropic-adaptive-thinking-model? "claude-sonnet-5-5"))
(check-true (anthropic-adaptive-thinking-model? "claude-haiku-5-5"))
(check-true (anthropic-adaptive-thinking-model? "claude-fable-5-2"))
(check-true (anthropic-adaptive-thinking-model? "claude-fable-5"))
(check-true (anthropic-adaptive-thinking-model? "claude-mythos-5-2"))

;; 4.6 是分界线
(check-true (anthropic-adaptive-thinking-model? "claude-opus-4-6"))
(check-true (anthropic-adaptive-thinking-model? "claude-sonnet-4-6"))
(check-true (anthropic-adaptive-thinking-model? "claude-opus-4-8"))
(check-false (anthropic-adaptive-thinking-model? "claude-opus-4-5"))
(check-false (anthropic-adaptive-thinking-model? "claude-sonnet-4-5"))
(check-false (anthropic-adaptive-thinking-model? "claude-haiku-4-5"))
(check-false (anthropic-adaptive-thinking-model? "claude-haiku-4-5-20251001"))
(check-false (anthropic-adaptive-thinking-model? "claude-3-7-sonnet-20250219"))

;; 前缀与大小写
(check-true (anthropic-adaptive-thinking-model? "anthropic/claude-opus-5-5"))
(check-true (anthropic-adaptive-thinking-model? "Claude-Sonnet-5-5"))

;; 非 Claude 模型（Anthropic 兼容端点）保持 budget_tokens
(check-false (anthropic-adaptive-thinking-model? "deepseek-v4-pro"))
(check-false (anthropic-adaptive-thinking-model? "MiniMax-M3"))
(check-false (anthropic-adaptive-thinking-model? "glm-5"))
(check-false (anthropic-adaptive-thinking-model? ""))
(check-false (anthropic-adaptive-thinking-model? #f))

(check-report)
