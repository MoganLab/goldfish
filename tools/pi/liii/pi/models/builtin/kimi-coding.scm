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

(define-library (liii pi models builtin kimi-coding)
  (import (scheme base))
  (export PI_KIMI_CODING_BUILTIN_MODELS)
  (begin

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
