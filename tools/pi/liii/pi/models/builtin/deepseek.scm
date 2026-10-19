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
