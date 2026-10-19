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
        (cond ((equal? p "anthropic") "claude-3-7-sonnet-20250219")
              ((equal? p "kimi-coding") "kimi-for-coding")
              ((equal? p "glm") "glm-4-plus")
              ((equal? p "deepseek") "deepseek-chat")
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
             (explicit-m
               (cond (has-cli-m? cli-m)
                     ((and (string? env-gf-m) (not (string-null? (string-trim-both env-gf-m))))
                      env-gf-m
                     ) ;
                     (else #f)
               ) ;cond
             ) ;explicit-m
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
                      (if has-cli-p?
                        (values cli-p
                          (or explicit-m
                            (%provider-env-model cli-p opt-env)
                            (and (equal? cli-p settings-p)
                              (string? settings-m)
                              (not (string-null? (string-trim-both settings-m)))
                              settings-m
                            ) ;and
                            (pi-provider-default-model cli-p)
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
          ;; 严格校验：模型必须属于选定的 provider（models-store 或内置目录）
          (if (pi-provider-has-model? provider model-id models-store)
            (values provider model-id)
            (%raise-model-provider-mismatch provider model-id)
          ) ;if
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
