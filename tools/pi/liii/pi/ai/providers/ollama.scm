;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers ollama)
  (import (scheme base))
  (export PI_OLLAMA_PROVIDER PI_OLLAMA_BUILTIN_MODELS)
  (begin

    ;; Provider 配置：api 为协议实现标识，baseUrl 为默认端点，
    ;; envKeys 为尝试的 API key 环境变量（本地模型免密，缺省回退虚拟 key）
    (define PI_OLLAMA_PROVIDER
      '((api . "openai-completions")
        (baseUrl . "http://localhost:11434/v1")
        (envKeys "OLLAMA_API_KEY"))
    ) ;define

    (define PI_OLLAMA_BUILTIN_MODELS
      '((("id" . "qwen2.5-coder:latest")
         ("name" . "Qwen 2.5 Coder Latest")
         ("baseUrl" . "http://localhost:11434/v1")
         ("contextWindow" . 32768)
         ("maxTokens" . 8192)
         ("reasoning" . #f)
         ("input" "text"))
        (("id" . "qwen3.5:9b")
         ("name" . "Qwen 3.5 9B")
         ("baseUrl" . "http://localhost:11434/v1")
         ("contextWindow" . 32768)
         ("maxTokens" . 8192)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "deepseek-coder-v2:latest")
         ("name" . "DeepSeek Coder V2 Latest")
         ("baseUrl" . "http://localhost:11434/v1")
         ("contextWindow" . 65536)
         ("maxTokens" . 8192)
         ("reasoning" . #f)
         ("input" "text")))
    ) ;define

  ) ;begin
) ;define-library
