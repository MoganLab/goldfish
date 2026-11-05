;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers deepseek)
  (import (scheme base))
  (export PI_DEEPSEEK_PROVIDER PI_DEEPSEEK_BUILTIN_MODELS)
  (begin

    ;; Provider 配置：api 为协议实现标识（对应 api/ 目录），baseUrl 为默认端点，
    ;; envKeys 为依次尝试的 API key 环境变量（对应上游 pi providers/<id>.ts 的角色）
    (define PI_DEEPSEEK_PROVIDER
      '((api . "anthropic-messages")
        (baseUrl . "https://api.deepseek.com/anthropic")
        (envKeys "DEEPSEEK_API_KEY"))
    ) ;define

    (define PI_DEEPSEEK_BUILTIN_MODELS
      '((("id" . "deepseek-v4-flash")
         ("name" . "DeepSeek V4 Flash")
         ("baseUrl" . "https://api.deepseek.com")
         ("contextWindow" . 1000000)
         ("maxTokens" . 384000)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "deepseek-v4-flash-vision-exp")
         ("name" . "DeepSeek V4 Flash Vision Exp")
         ("baseUrl" . "https://api.deepseek.com")
         ("contextWindow" . 1000000)
         ("maxTokens" . 384000)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "deepseek-v4-pro")
         ("name" . "DeepSeek V4 Pro")
         ("baseUrl" . "https://api.deepseek.com")
         ("contextWindow" . 1000000)
         ("maxTokens" . 384000)
         ("reasoning" . #t)
         ("input" "text")))
    ) ;define

  ) ;begin
) ;define-library
