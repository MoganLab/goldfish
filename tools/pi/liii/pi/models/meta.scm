;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi models meta)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii pi template)
    (liii pi config types)
    (liii pi config record)
    (liii pi models builtin)
  ) ;import
  (export pi-resolve-model-metadata)
  (begin

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

    (define (%find-model-in-list models model-id)
      (and (pair? models)
        (let loop
          ((lst models))
          (cond ((null? lst) #f)
                ((string=? (%dict-ref (car lst) "id" "") model-id) (car lst))
                (else (loop (cdr lst)))
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    (define (%get-provider-models models-store provider)
      (let ((raw-models (%dict-ref (%dict-ref models-store provider '()) "models" '())))
        (cond ((vector? raw-models) (vector->list raw-models))
              ((list? raw-models) raw-models)
              (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (%apply-meta-env-overrides base-url context-window max-tokens opt-env provider)
      (let* ((env-url
               (or
                 (and (string? provider)
                   (string=? provider "deepseek")
                   (or (lookup-env-var "DEEPSEEK_BASE_URL" opt-env)
                     (let ((ant-url (lookup-env-var "ANTHROPIC_BASE_URL" opt-env)))
                       (and (string? ant-url) (string-contains? ant-url "deepseek") ant-url)
                     ) ;let
                   ) ;or
                 ) ;and
                 (and (string? provider)
                   (or (string=? provider "anthropic") (string=? provider "glm"))
                   (lookup-env-var "ANTHROPIC_BASE_URL" opt-env)
                 ) ;and
                 (and (string? provider)
                   (or (string=? provider "minimax") (string=? provider "minimax-cn"))
                   (or (lookup-env-var (if (string=? provider "minimax-cn") "MINIMAX_CN_BASE_URL" "MINIMAX_BASE_URL")
                         opt-env
                       ) ;lookup-env-var
                     (lookup-env-var "MINIMAX_BASE_URL" opt-env)
                   ) ;or
                 ) ;and
                 (lookup-env-var "GF_PI_BASE_URL" opt-env)
               ) ;or
             ) ;env-url
             (env-cw (lookup-env-var "GF_PI_CONTEXT_WINDOW" opt-env))
             (env-mt (lookup-env-var "GF_PI_MAX_TOKENS" opt-env))
             (final-url
               (if (and (string? env-url) (not (string-null? env-url))) env-url base-url)
             ) ;final-url
             (final-cw (if (and (string? env-cw) (string->number env-cw))
                         (string->number env-cw)
                         context-window
                       ) ;if
             ) ;final-cw
             (final-mt (if (and (string? env-mt) (string->number env-mt))
                         (string->number env-mt)
                         max-tokens
                       ) ;if
             ) ;final-mt
            ) ;
        (values final-url final-cw final-mt)
      ) ;let*
    ) ;define

    (define (pi-resolve-model-metadata provider model-id models-store . opt-env)
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (is-ant? (string=? p-str "anthropic"))
             (is-kimi? (string=? p-str "kimi-coding"))
             (is-glm? (string=? p-str "glm"))
             (is-ds? (string=? p-str "deepseek"))
             (is-mm? (or (string=? p-str "minimax") (string=? p-str "minimax-cn")))
             (is-zai? (or (string=? p-str "zai-coding-cn") (string=? p-str "zai")))
             (def-url (cond (is-ant? "https://api.anthropic.com")
                            (is-kimi? "https://api.kimi.com/coding/v1")
                            (is-glm? "https://open.bigmodel.cn/api/anthropic")
                            (is-ds? "https://api.deepseek.com/anthropic")
                            (is-mm? "https://api.minimaxi.com/anthropic")
                            (is-zai? "https://open.bigmodel.cn/api/coding/paas/v4")
                            (else "https://generativelanguage.googleapis.com/v1beta")
                      ) ;cond
             ) ;def-url
             (m
               (or (%find-model-in-list (%get-provider-models models-store p-str) model-id)
                 (and (or is-mm? is-zai?)
                   (%find-model-in-list (%dict-ref PI_PROVIDER_BUILTIN_MODELS p-str '()) model-id)
                 ) ;and
               ) ;or
             ) ;m
             (raw-url (if m (%dict-ref m "baseUrl" def-url) def-url))
             (norm-url
               (cond ((and is-kimi? (string=? raw-url "https://api.kimi.com/coding"))
                      "https://api.kimi.com/coding/v1"
                     ) ;
                     ((and is-mm?
                        (or (string=? raw-url "https://api.minimax.io")
                          (string=? raw-url "https://api.minimax.io/anthropic")
                        ) ;or
                      ) ;and
                      "https://api.minimaxi.com/anthropic"
                     ) ;
                     ((and is-ds? (string=? raw-url "https://api.deepseek.com"))
                      "https://api.deepseek.com/anthropic"
                     ) ;
                     (else raw-url)
               ) ;cond
             ) ;norm-url
             (def-cw (cond (is-ant? 200000)
                           (is-kimi? 262144)
                           (is-mm? 1000000)
                           (is-zai? 1000000)
                           (else 128000)
                     ) ;cond
             ) ;def-cw
             (def-mt (cond (is-ant? 8192)
                           (is-kimi? 8192)
                           (is-glm? 8192)
                           (is-ds? 8192)
                           (is-mm? 512000)
                           (is-zai? 131072)
                           (else 16384)
                     ) ;cond
             ) ;def-mt
             (cw (if m (%dict-ref m "contextWindow" def-cw) def-cw))
             (mt (if m (%dict-ref m "maxTokens" def-mt) def-mt))
            ) ;
        (let-values (((f-url f-cw f-mt) (%apply-meta-env-overrides norm-url cw mt opt-env p-str)))
          (%make-pi-model-metadata model-id
            (if m (%dict-ref m "name" model-id) model-id)
            p-str
            f-url
            f-cw
            f-mt
            (if m #t #f)
          ) ;%make-pi-model-metadata
        ) ;let-values
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
