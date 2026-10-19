;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi models builtin)
  (import (scheme base))
  (export PI_SUPPORTED_PROVIDERS PI_PROVIDER_BUILTIN_MODELS)
  (begin

    (define PI_SUPPORTED_PROVIDERS
      '("google" "anthropic" "kimi-coding" "glm" "deepseek")
    ) ;define

    (define PI_PROVIDER_BUILTIN_MODELS
      '(("google"
         (("id" . "gemini-3.8-flash")
          ("name" . "Gemini 3.8 Flash")
          ("baseUrl" . "https://generativelanguage.googleapis.com/v1beta")
          ("contextWindow" . 1048576)
          ("maxTokens" . 65536))
         (("id" . "gemini-2.5-flash")
          ("name" . "Gemini 2.5 Flash")
          ("baseUrl" . "https://generativelanguage.googleapis.com/v1beta")
          ("contextWindow" . 1048576)
          ("maxTokens" . 65536))
         (("id" . "gemini-2.5-pro")
          ("name" . "Gemini 2.5 Pro")
          ("baseUrl" . "https://generativelanguage.googleapis.com/v1beta")
          ("contextWindow" . 2097152)
          ("maxTokens" . 65536)))
        ("anthropic"
         (("id" . "claude-3-7-sonnet-20250219")
          ("name" . "Claude 3.7 Sonnet")
          ("baseUrl" . "https://api.anthropic.com")
          ("contextWindow" . 200000)
          ("maxTokens" . 8192))
         (("id" . "claude-3-5-sonnet-20241022")
          ("name" . "Claude 3.5 Sonnet")
          ("baseUrl" . "https://api.anthropic.com")
          ("contextWindow" . 200000)
          ("maxTokens" . 8192)))
        ("kimi-coding"
         (("id" . "kimi-for-coding")
          ("name" . "Kimi for Coding")
          ("baseUrl" . "https://api.kimi.com/coding/v1")
          ("contextWindow" . 262144)
          ("maxTokens" . 8192))
         (("id" . "k3")
          ("name" . "Kimi K3")
          ("baseUrl" . "https://api.kimi.com/coding/v1")
          ("contextWindow" . 262144)
          ("maxTokens" . 8192)))
        ("glm"
         (("id" . "glm-4-plus")
          ("name" . "GLM-4-Plus")
          ("baseUrl" . "https://open.bigmodel.cn/api/anthropic")
          ("contextWindow" . 128000)
          ("maxTokens" . 8192)))
        ("deepseek"
         (("id" . "deepseek-chat")
          ("name" . "DeepSeek-Chat")
          ("baseUrl" . "https://api.deepseek.com/anthropic")
          ("contextWindow" . 128000)
          ("maxTokens" . 8192))
         (("id" . "deepseek-reasoner")
          ("name" . "DeepSeek-Reasoner")
          ("baseUrl" . "https://api.deepseek.com/anthropic")
          ("contextWindow" . 128000)
          ("maxTokens" . 8192))))
    ) ;define

  ) ;begin
) ;define-library
