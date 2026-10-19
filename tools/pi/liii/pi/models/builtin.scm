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
  (import (scheme base)
    (liii pi models builtin anthropic)
    (liii pi models builtin deepseek)
    (liii pi models builtin google)
    (liii pi models builtin kimi-coding)
    (liii pi models builtin minimax)
    (liii pi models builtin minimax-cn)
    (liii pi models builtin zai-coding-cn)
  ) ;import
  (export PI_SUPPORTED_PROVIDERS PI_PROVIDER_BUILTIN_MODELS
    pi-builtin-augment-store pi-builtin-augment-all
  ) ;export
  (begin

    (define PI_SUPPORTED_PROVIDERS
      '("google" "anthropic" "kimi-coding" "glm" "deepseek" "minimax"
        "minimax-cn" "zai-coding-cn")
    ) ;define

    (define PI_PROVIDER_BUILTIN_MODELS
      (list (cons "anthropic" PI_ANTHROPIC_BUILTIN_MODELS)
        (cons "deepseek" PI_DEEPSEEK_BUILTIN_MODELS)
        (cons "google" PI_GOOGLE_BUILTIN_MODELS)
        (cons "kimi-coding" PI_KIMI_CODING_BUILTIN_MODELS)
        (cons "minimax" PI_MINIMAX_BUILTIN_MODELS)
        (cons "minimax-cn" PI_MINIMAX_CN_BUILTIN_MODELS)
        (cons "zai-coding-cn" PI_ZAI_CODING_CN_BUILTIN_MODELS)
      ) ;list
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

    ;; 对全部内置 provider 执行缺失模型补齐
    (define (pi-builtin-augment-all models-store)
      (let loop
        ((rest PI_PROVIDER_BUILTIN_MODELS) (acc models-store))
        (if (null? rest)
          acc
          (loop (cdr rest) (pi-builtin-augment-store acc (caar rest)))
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
