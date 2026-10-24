;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers minimax-cn)
  (import (scheme base))
  (export PI_MINIMAX_CN_PROVIDER PI_MINIMAX_CN_BUILTIN_MODELS)
  (begin

    ;; Provider 配置：api 为协议实现标识（对应 api/ 目录），baseUrl 为默认端点，
    ;; envKeys 为依次尝试的 API key 环境变量（对应上游 pi providers/<id>.ts 的角色）
    (define PI_MINIMAX_CN_PROVIDER
      '((api . "anthropic-messages")
        (baseUrl . "https://api.minimaxi.com/anthropic")
        (envKeys "MINIMAX_CN_API_KEY" "MINIMAX_API_KEY"))
    ) ;define

    (define PI_MINIMAX_CN_BUILTIN_MODELS
      '((("id" . "MiniMax-M3.1-Flash")
         ("name" . "MiniMax-M3.1-Flash")
         ("baseUrl" . "https://api.minimaxi.com/anthropic")
         ("contextWindow" . 1000000)
         ("maxTokens" . 524288)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "MiniMax-M3")
         ("name" . "MiniMax-M3")
         ("baseUrl" . "https://api.minimaxi.com/anthropic")
         ("contextWindow" . 1048576)
         ("maxTokens" . 512000)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "MiniMax-M2.7")
         ("name" . "MiniMax-M2.7")
         ("baseUrl" . "https://api.minimaxi.com/anthropic")
         ("contextWindow" . 204800)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "MiniMax-M2.7-highspeed")
         ("name" . "MiniMax-M2.7-highspeed")
         ("baseUrl" . "https://api.minimaxi.com/anthropic")
         ("contextWindow" . 204800)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text")))
    ) ;define

  ) ;begin
) ;define-library
