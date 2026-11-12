;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers all-in-one)
  (import (scheme base))
  (export PI_ALL_IN_ONE_PROVIDER PI_ALL_IN_ONE_BUILTIN_MODELS)
  (begin

    (define PI_ALL_IN_ONE_PROVIDER '((api . "all-in-one") (envKeys)))

    (define PI_ALL_IN_ONE_BUILTIN_MODELS
      '((("id" . "coding-fast")
         ("name" . "快速")
         ("contextWindow" . 1000000)
         ("maxTokens" . 65536)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "coding-high-quality")
         ("name" . "高质量")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text")))
    ) ;define

  ) ;begin
) ;define-library
