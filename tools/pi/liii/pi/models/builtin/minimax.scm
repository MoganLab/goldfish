;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi models builtin minimax)
  (import (scheme base))
  (export PI_MINIMAX_BUILTIN_MODELS)
  (begin

    (define PI_MINIMAX_BUILTIN_MODELS
      '((("id" . "MiniMax-M2.7")
         ("name" . "MiniMax-M2.7")
         ("baseUrl" . "https://api.minimax.io/anthropic")
         ("contextWindow" . 204800)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "MiniMax-M2.7-highspeed")
         ("name" . "MiniMax-M2.7-highspeed")
         ("baseUrl" . "https://api.minimax.io/anthropic")
         ("contextWindow" . 204800)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "MiniMax-M3")
         ("name" . "MiniMax-M3")
         ("baseUrl" . "https://api.minimax.io/anthropic")
         ("contextWindow" . 1048576)
         ("maxTokens" . 512000)
         ("reasoning" . #t)
         ("input" "text" "image")))
    ) ;define

  ) ;begin
) ;define-library
