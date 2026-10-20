;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi models route)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii pi config types)
    (liii pi config record)
    (liii pi models builtin)
  ) ;import
  (export pi-provider-default-model pi-provider-has-model?
    pi-find-model-provider pi-route-provider-and-model pi-ensure-catalog-meta
  ) ;export
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

    (define (pi-provider-default-model provider)
      (let ((p (if (symbol? provider) (symbol->string provider) provider)))
        (cond ((equal? p "anthropic") "claude-opus-4-8")
              ((equal? p "kimi-coding") "kimi-for-coding")
              ((equal? p "glm") "glm-5.3")
              ((equal? p "deepseek") "deepseek-v4-pro")
              ((or (equal? p "zai-coding-cn") (equal? p "zai")) "glm-5.3")
              ((or (equal? p "minimax") (equal? p "minimax-cn")) "MiniMax-M3.1-Flash")
              (else "gemini-3.8-flash")
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-provider-has-model? provider model-id models-store)
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (store-models (%get-provider-models models-store p-str))
             (store-match (%find-model-in-list store-models model-id))
            ) ;
        (if
          (or store-match
            (let ((builtin (%dict-ref PI_PROVIDER_BUILTIN_MODELS p-str '())))
              (%find-model-in-list builtin model-id)
            ) ;let
          ) ;or
          #t
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (%provider-first-model-id provider models-store)
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (store-models (%get-provider-models models-store p-str))
            ) ;
        (if (pair? store-models)
          (%dict-ref (car store-models) "id" #f)
          (let ((builtin (%dict-ref PI_PROVIDER_BUILTIN_MODELS p-str '())))
            (if (pair? builtin) (%dict-ref (car builtin) "id" #f) #f)
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-find-model-provider model-id models-store . opt-pref)
      (let* ((pref (and (pair? opt-pref) (car opt-pref)))
             (pref-str (and (string? pref) (not (string-null? pref)) pref))
            ) ;
        (cond ((and pref-str (pi-provider-has-model? pref-str model-id models-store))
               pref-str
              ) ;
              (else
                (let loop
                  ((lst PI_SUPPORTED_PROVIDERS))
                  (cond ((null? lst) #f)
                        ((pi-provider-has-model? (car lst) model-id models-store) (car lst))
                        (else (loop (cdr lst)))
                  ) ;cond
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (%safe-lookup-env name opt-env)
      (let ((has-custom?
              (and (pair? opt-env) (or (pair? (car opt-env)) (null? (car opt-env))))
            ) ;has-custom?
            (custom-alist
              (cond
               ((and (pair? opt-env) (pair? (car opt-env)) (pair? (caar opt-env)))
                (car opt-env)
               ) ;
               ((and (pair? opt-env) (pair? (car opt-env)) (string? (caar opt-env))) opt-env)
               (else #f)
              ) ;cond
            ) ;custom-alist
           ) ;
        (cond
          (custom-alist
            (let ((pair (assoc name custom-alist)))
              (if pair (cdr pair) #f)
            ) ;let
          ) ;custom-alist
          (has-custom? #f)
          (else (get-environment-variable name))
        ) ;cond
      ) ;let
    ) ;define

    (define (%provider-env-model provider opt-env)
      (cond ((equal? provider "anthropic")
             (%safe-lookup-env "ANTHROPIC_DEFAULT_SONNET_MODEL" opt-env)
            ) ;
            ((equal? provider "glm") (%safe-lookup-env "GLM_MODEL" opt-env))
            ((equal? provider "deepseek") (%safe-lookup-env "DEEPSEEK_MODEL" opt-env))
            ((equal? provider "minimax") (%safe-lookup-env "MINIMAX_MODEL" opt-env))
            ((equal? provider "minimax-cn") (%safe-lookup-env "MINIMAX_CN_MODEL" opt-env))
            (else #f)
      ) ;cond
    ) ;define

    (define (%settings-ref settings sym-key str-key default-val)
      (cond
       ((pi-settings? settings)
        (case sym-key
         ((default-provider) (pi-settings-default-provider settings))
         ((default-model) (pi-settings-default-model settings))
         (else default-val)
        ) ;case
       ) ;
       ((pair? settings)
        (let ((pair (or (assoc sym-key settings) (assoc str-key settings))))
          (if (pair? pair) (cdr pair) default-val)
        ) ;let
       ) ;
       (else default-val)
      ) ;cond
    ) ;define

    (define (%raise-unknown-model model-id)
      (error 'pi-config-error
        (string-append "Unknown model \""
          model-id
          "\": not found in any configured provider model catalog. Use --list-models to see available models, or add the model to models.json in the agent dir."
        ) ;string-append
      ) ;error
    ) ;define

    (define (%raise-model-provider-mismatch provider model-id)
      (error 'pi-config-error
        (string-append "Model \"" model-id "\" is not available for provider \""
          provider
          "\". Use --list-models to see available models, or add the model to models.json in the agent dir."
        ) ;string-append
      ) ;error
    ) ;define

    (define (pi-route-provider-and-model cli-p cli-m settings models-store opt-env)
      (let* ((has-cli-p?
               (and (string? cli-p) (not (string-null? (string-trim-both cli-p))))
             ) ;has-cli-p?
             (has-cli-m?
               (and (string? cli-m) (not (string-null? (string-trim-both cli-m))))
             ) ;has-cli-m?
             (env-gf-m (%safe-lookup-env "GF_PI_MODEL" opt-env))
             (raw-explicit-m
               (cond (has-cli-m? cli-m)
                     ((and (string? env-gf-m) (not (string-null? (string-trim-both env-gf-m))))
                      env-gf-m
                     ) ;
                     (else #f)
               ) ;cond
             ) ;raw-explicit-m
             (slash-parts
               (if (and (string? raw-explicit-m) (string-contains? raw-explicit-m "/"))
                 (string-split raw-explicit-m "/")
                 #f
               ) ;if
             ) ;slash-parts
             (slash-p (and slash-parts (pair? slash-parts) (car slash-parts)))
             (explicit-m
               (if (and slash-parts (pair? (cdr slash-parts)))
                 (cadr slash-parts)
                 raw-explicit-m
               ) ;if
             ) ;explicit-m
             (effective-p (or (and has-cli-p? cli-p) slash-p))
             (has-effective-p?
               (and (string? effective-p) (not (string-null? (string-trim-both effective-p))))
             ) ;has-effective-p?
             (settings-p (%settings-ref settings 'default-provider "default-provider" #f))
             (settings-m (%settings-ref settings 'default-model "default-model" #f))
             (default-p
               (if
                 (and (string? settings-p) (not (string-null? (string-trim-both settings-p))))
                 settings-p
                 "google"
               ) ;if
             ) ;default-p
            ) ;
        (let-values (((provider model-id)
                      (if has-effective-p?
                        (values effective-p
                          (or explicit-m
                            (%provider-env-model effective-p opt-env)
                            (and (equal? effective-p settings-p)
                              (string? settings-m)
                              (not (string-null? (string-trim-both settings-m)))
                              settings-m
                            ) ;and
                            (pi-provider-default-model effective-p)
                          ) ;or
                        ) ;values
                        (if explicit-m
                          (let ((found-p (pi-find-model-provider explicit-m models-store default-p)))
                            (if found-p (values found-p explicit-m) (%raise-unknown-model explicit-m))
                          ) ;let
                          (values default-p
                            (or (%provider-env-model default-p opt-env)
                              (and (string? settings-m)
                                (not (string-null? (string-trim-both settings-m)))
                                settings-m
                              ) ;and
                              (pi-provider-default-model default-p)
                            ) ;or
                          ) ;values
                        ) ;if
                      ) ;if
                     ) ;
                    ) ;
          ;; 校验模型：若非显式传入且默认模型不在目录内，按 buildFallbackModel 回退到 provider 第一个模型
          (let ((is-default-m? (and (not explicit-m) (not (%provider-env-model provider opt-env)))
                ) ;is-default-m?
               ) ;
            (cond ((pi-provider-has-model? provider model-id models-store)
                   (values provider model-id)
                  ) ;
                  (is-default-m?
                    (let ((fallback (%provider-first-model-id provider models-store)))
                      (if (and fallback (pi-provider-has-model? provider fallback models-store))
                        (values provider fallback)
                        (%raise-model-provider-mismatch provider model-id)
                      ) ;if
                    ) ;let
                  ) ;is-default-m?
                  (else (%raise-model-provider-mismatch provider model-id))
            ) ;cond
          ) ;let
        ) ;let-values
      ) ;let*
    ) ;define

    (define (pi-ensure-catalog-meta meta provider model-id models-store)
      (if (and (not (pi-model-metadata-matched? meta))
            (pi-provider-has-model? provider model-id models-store)
          ) ;and
        (%make-pi-model-metadata (pi-model-metadata-id meta)
          (pi-model-metadata-name meta)
          (pi-model-metadata-provider meta)
          (pi-model-metadata-base-url meta)
          (pi-model-metadata-context-window meta)
          (pi-model-metadata-max-tokens meta)
          #t
        ) ;%make-pi-model-metadata
        meta
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
