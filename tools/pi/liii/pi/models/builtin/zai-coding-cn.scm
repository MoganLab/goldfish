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

(define-library (liii pi models builtin zai-coding-cn)
  (import (scheme base))
  (export PI_ZAI_CODING_CN_BUILTIN_MODELS)
  (begin

    (define PI_ZAI_CODING_CN_BUILTIN_MODELS
      '((("id" . "glm-4.6v")
         ("name" . "GLM-4.6V")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 128000)
         ("maxTokens" . 32768)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "glm-4.7")
         ("name" . "GLM-4.7")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 204800)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5-turbo")
         ("name" . "GLM-5-Turbo")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 200000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.1")
         ("name" . "GLM-5.1")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 200000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.2")
         ("name" . "GLM-5.2")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.2-highspeed")
         ("name" . "GLM-5.2 Highspeed")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.3")
         ("name" . "GLM-5.3")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.3-flash")
         ("name" . "GLM-5.3-Flash")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "glm-5.3-highspeed")
         ("name" . "GLM-5.3 Highspeed")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5v-turbo")
         ("name" . "GLM-5V-Turbo")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 200000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text" "image")))
    ) ;define

  ) ;begin
) ;define-library
