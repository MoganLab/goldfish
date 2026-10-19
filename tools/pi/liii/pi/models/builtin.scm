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
  (export PI_SUPPORTED_PROVIDERS
    PI_PROVIDER_BUILTIN_MODELS
    pi-builtin-augment-store
  ) ;export
  (begin

    (define PI_SUPPORTED_PROVIDERS
      '("google" "anthropic" "kimi-coding" "glm" "deepseek" "minimax"
        "minimax-cn")
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
          ("maxTokens" . 8192)))
        ("minimax"
         (("id" . "MiniMax-M3.1-Flash")
          ("name" . "MiniMax-M3.1-Flash")
          ("baseUrl" . "https://api.minimaxi.com/anthropic")
          ("contextWindow" . 1000000)
          ("maxTokens" . 524288)
          ("reasoning" . #t)
          ("input" "text" "image"))
         (("id" . "MiniMax-M3")
          ("name" . "MiniMax-M3")
          ("baseUrl" . "https://api.minimaxi.com/anthropic")
          ("contextWindow" . 1000000)
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
        ("minimax-cn"
         (("id" . "MiniMax-M3.1-Flash")
          ("name" . "MiniMax-M3.1-Flash")
          ("baseUrl" . "https://api.minimaxi.com/anthropic")
          ("contextWindow" . 1000000)
          ("maxTokens" . 524288)
          ("reasoning" . #t)
          ("input" "text" "image"))
         (("id" . "MiniMax-M3")
          ("name" . "MiniMax-M3")
          ("baseUrl" . "https://api.minimaxi.com/anthropic")
          ("contextWindow" . 1000000)
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
          ("input" "text"))))
    ) ;define

    (define (%dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%models-of entry)
      (let ((raw (%dict-ref (cdr entry) "models" '())))
        (cond ((vector? raw) (vector->list raw))
              ((list? raw) raw)
              (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (%model-ids models)
      (map (lambda (m) (%dict-ref m "id" "")) models)
    ) ;define

    (define (%missing-models store-models builtin-models)
      (let ((store-ids (%model-ids store-models)))
        (let loop
          ((rest builtin-models) (acc '()))
          (cond ((null? rest) (reverse acc))
                ((member (%dict-ref (car rest) "id" "") store-ids) (loop (cdr rest) acc))
                (else (loop (cdr rest) (cons (car rest) acc)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%provider-entry-name entry)
      (let ((n (car entry)))
        (if (string? n) n (symbol->string n))
      ) ;let
    ) ;define

    (define (%strip-models-field fields)
      (let loop
        ((rest fields) (acc '()))
        (cond ((null? rest) (reverse acc))
              ((let ((k (caar rest)))
                 (or (equal? k "models") (eq? k 'models))
               ) ;let
               (loop (cdr rest) acc)
              ) ;
              (else (loop (cdr rest) (cons (car rest) acc)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%merge-entry-models entry store-models missing)
      (cons (car entry)
        (cons (cons "models" (append store-models missing))
          (%strip-models-field (cdr entry))
        ) ;cons
      ) ;cons
    ) ;define

    (define (%find-provider-entry models-store p-str)
      (let loop
        ((rest models-store))
        (cond ((null? rest) #f)
              ((equal? (%provider-entry-name (car rest)) p-str) (car rest))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 把 provider 的内置模型目录补进 models-store。已有条目保留原顺序，
    ;; 缺失的内置模型按键序追加；未知 provider 原样返回。
    (define (pi-builtin-augment-store models-store provider)
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (builtin (%dict-ref PI_PROVIDER_BUILTIN_MODELS p-str '()))
            ) ;
        (if (not (pair? builtin))
          models-store
          (let* ((entry (%find-provider-entry models-store p-str))
                 (store-models (if entry (%models-of entry) '()))
                 (missing (%missing-models store-models builtin))
                ) ;
            (cond ((null? missing) models-store)
                  ((not entry) (cons (cons p-str (list (cons "models" builtin))) models-store))
                  (else
                    (map (lambda (e)
                           (if (eq? e entry) (%merge-entry-models entry store-models missing) e)
                         ) ;lambda
                      models-store
                    ) ;map
                  ) ;else
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
