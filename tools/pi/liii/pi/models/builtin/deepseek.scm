;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi models builtin deepseek)
  (import (scheme base))
  (export PI_DEEPSEEK_BUILTIN_MODELS)
  (begin

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
