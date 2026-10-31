;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers kimi-coding)
  (import (scheme base))
  (export PI_KIMI_CODING_PROVIDER PI_KIMI_CODING_BUILTIN_MODELS)
  (begin

    ;; Provider 配置：api 为协议实现标识（对应 api/ 目录），baseUrl 为默认端点，
    ;; envKeys 为依次尝试的 API key 环境变量（对应上游 pi providers/<id>.ts 的角色），
    ;; quotaErrorCodes 为「额度耗尽」标识表。
    ;;
    ;; Kimi Coding Plan 用 error.type（而不是 code）区分额度耗尽：
    ;;   {"error":{"type":"exceeded_current_quota_error","message":"You've reached
    ;;    your weekly (7-day) usage limit. ..."}}
    (define PI_KIMI_CODING_PROVIDER
      '((api . "openai-completions")
        (baseUrl . "https://api.kimi.com/coding/v1")
        (envKeys "KIMI_API_KEY")
        (quotaErrorCodes ("exceeded_current_quota_error" . "quota")))
    ) ;define

    (define PI_KIMI_CODING_BUILTIN_MODELS
      '((("id" . "k3")
         ("name" . "Kimi K3")
         ("baseUrl" . "https://api.kimi.com/coding")
         ("contextWindow" . 1048576)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "k3-256k")
         ("name" . "Kimi K3-256K")
         ("baseUrl" . "https://api.kimi.com/coding")
         ("contextWindow" . 262144)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "kimi-for-coding")
         ("name" . "Kimi K2.7 Code")
         ("baseUrl" . "https://api.kimi.com/coding")
         ("contextWindow" . 262144)
         ("maxTokens" . 32768)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "kimi-for-coding-highspeed")
         ("name" . "Kimi For Coding HighSpeed")
         ("baseUrl" . "https://api.kimi.com/coding")
         ("contextWindow" . 262144)
         ("maxTokens" . 32768)
         ("reasoning" . #t)
         ("input" "text" "image")))
    ) ;define

  ) ;begin
) ;define-library
