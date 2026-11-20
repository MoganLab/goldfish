;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers anthropic)
  (import (scheme base))
  (export PI_ANTHROPIC_PROVIDER PI_ANTHROPIC_BUILTIN_MODELS)
  (begin

    ;; Provider 配置：Anthropic 官方 Messages API（x-api-key 认证）。
    ;; 思考参数按模型选择格式，见 (liii pi ai api anthropic-messages thinking)。
    (define PI_ANTHROPIC_PROVIDER
      '((api . "anthropic-messages")
        (baseUrl . "https://api.anthropic.com")
        (envKeys "ANTHROPIC_API_KEY"))
    ) ;define

    ;; 当前在售的 Claude 模型（2026-11，以 GET /v1/models 为准）：
    ;; 上下文 1M、单次输出上限 128K，均支持图片输入与自适应思考。
    (define PI_ANTHROPIC_BUILTIN_MODELS
      '((("id" . "claude-opus-5-5")
         ("name" . "Claude Opus 5.5")
         ("baseUrl" . "https://api.anthropic.com")
         ("contextWindow" . 1000000)
         ("maxTokens" . 128000)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "claude-sonnet-5-5")
         ("name" . "Claude Sonnet 5.5")
         ("baseUrl" . "https://api.anthropic.com")
         ("contextWindow" . 1000000)
         ("maxTokens" . 128000)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "claude-haiku-5-5")
         ("name" . "Claude Haiku 5.5")
         ("baseUrl" . "https://api.anthropic.com")
         ("contextWindow" . 1000000)
         ("maxTokens" . 128000)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "claude-fable-5-2")
         ("name" . "Claude Fable 5.2")
         ("baseUrl" . "https://api.anthropic.com")
         ("contextWindow" . 1000000)
         ("maxTokens" . 128000)
         ("reasoning" . #t)
         ("input" "text" "image")))
    ) ;define

  ) ;begin
) ;define-library
