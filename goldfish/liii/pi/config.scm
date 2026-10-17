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

(define-library (liii pi config)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii path)
    (liii os)
    (liii json)
    (liii error)
    (liii pi read)
    (liii pi template)
  ) ;import
  (export pi-resolve-config-value json-dict-ref pi-get-agent-dir
    pi-read-json-file pi-load-settings pi-resolve-proxy-url pi-load-auth
    pi-resolve-provider-key pi-load-models-store pi-resolve-model-metadata
  ) ;export
  (begin

    (define (json-dict-ref obj key default-val)
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

    (define (pi-get-agent-dir . opt-dir)
      (let ((custom (if (pair? opt-dir) (car opt-dir) #f))
            (env-dir (get-environment-variable "PI_CODING_AGENT_DIR"))
           ) ;
        (cond
         ((and (string? custom) (not (string-null? (string-trim-both custom))))
          (pi-resolve-path custom (getcwd))
         ) ;
         ((and (string? env-dir) (not (string-null? (string-trim-both env-dir))))
          (pi-resolve-path env-dir (getcwd))
         ) ;
         (else (pi-resolve-path "~/.pi/agent" (getcwd)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-read-json-file filepath)
      (if (not (path-exists? (path filepath)))
        #f
        (catch #t
          (lambda () (string->json (path-read-text (path filepath))))
          (lambda args #f)
        ) ;catch
      ) ;if
    ) ;define

    (define (%normalize-bool val)
      (cond ((eq? val 'true) #t)
            ((eq? val 'false) #f)
            (else val)
      ) ;cond
    ) ;define

    (define (%parse-tools-list val)
      (cond ((vector? val) (vector->list val))
            ((list? val) val)
            (else #f)
      ) ;cond
    ) ;define

    (define (%build-retry-alist retry-obj)
      (list (cons 'enabled (%normalize-bool (json-dict-ref retry-obj "enabled" #t)))
        (cons 'max-retries (json-dict-ref retry-obj "maxRetries" 3))
        (cons 'base-delay-ms (json-dict-ref retry-obj "baseDelayMs" 2000))
        (cons 'max-agent-delay-ms (json-dict-ref retry-obj "maxAgentDelayMs" 60000))
      ) ;list
    ) ;define

    (define (pi-load-settings agent-dir)
      (let* ((filepath (path->string (path-join (path agent-dir) (path "settings.json"))))
             (data (pi-read-json-file filepath))
             (obj (if (pair? data) data '()))
             (raw-proxy (json-dict-ref obj "httpProxy" #f))
             (clean-proxy
               (if
                 (and (string? raw-proxy) (not (string-null? (string-trim-both raw-proxy))))
                 raw-proxy
                 #f
               ) ;if
             ) ;clean-proxy
            ) ;
        (list (cons 'default-provider (json-dict-ref obj "defaultProvider" "google"))
          (cons 'default-model (json-dict-ref obj "defaultModel" "gemini-3.8-flash"))
          (cons 'default-thinking-level
            (json-dict-ref obj "defaultThinkingLevel" "medium")
          ) ;cons
          (cons 'http-proxy clean-proxy)
          (cons 'retry (%build-retry-alist (json-dict-ref obj "retry" '())))
          (cons 'default-tools (%parse-tools-list (json-dict-ref obj "defaultTools" #f)))
        ) ;list
      ) ;let*
    ) ;define

    (define (pi-resolve-proxy-url cli-proxy settings . opt-env)
      (let ((env-https (lookup-env-var "HTTPS_PROXY" opt-env))
            (env-all (lookup-env-var "ALL_PROXY" opt-env))
            (settings-proxy
              (let ((p (assoc 'http-proxy settings)))
                (if p (cdr p) #f)
              ) ;let
            ) ;settings-proxy
           ) ;
        (cond
         ((and (string? cli-proxy) (not (string-null? (string-trim-both cli-proxy))))
          cli-proxy
         ) ;
         ((and (string? env-https) (not (string-null? (string-trim-both env-https))))
          env-https
         ) ;
         ((and (string? env-all) (not (string-null? (string-trim-both env-all))))
          env-all
         ) ;
         ((and (string? settings-proxy)
            (not (string-null? (string-trim-both settings-proxy)))
          ) ;and
          settings-proxy
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%parse-credential-item key val opt-env)
      (if (and (pair? val) (equal? (json-dict-ref val "type" #f) "api_key"))
        (let ((raw-key (json-dict-ref val "key" #f)))
          (if (not (string? raw-key))
            #f
            (let ((resolved (if (pair? opt-env)
                              (pi-resolve-config-value raw-key (car opt-env))
                              (pi-resolve-config-value raw-key)
                            ) ;if
                  ) ;resolved
                 ) ;
              (if resolved (cons (if (symbol? key) (symbol->string key) key) resolved) #f)
            ) ;let
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (pi-load-auth agent-dir . opt-env)
      (let* ((filepath (path->string (path-join (path agent-dir) (path "auth.json"))))
             (data (pi-read-json-file filepath))
            ) ;
        (if (not (pair? data))
          '()
          (let loop
            ((items data) (acc '()))
            (if (null? items)
              (reverse acc)
              (let* ((entry (car items))
                     (pair (%parse-credential-item (car entry) (cdr entry) opt-env))
                    ) ;
                (if pair (loop (cdr items) (cons pair acc)) (loop (cdr items) acc))
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-resolve-provider-key provider auth-alist . opt-env)
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (pair (or (assoc p-str auth-alist) (assoc (string->symbol p-str) auth-alist)))
            ) ;
        (cond
         ((and pair (string? (cdr pair)) (not (string-null? (cdr pair)))) (cdr pair))
         ((string=? p-str "google")
          (let ((env-val (lookup-env-var "GEMINI_API_KEY" opt-env)))
            (if (and (string? env-val) (not (string-null? env-val))) env-val #f)
          ) ;let
         ) ;
         ((string=? p-str "anthropic")
          (let ((env-val (lookup-env-var "ANTHROPIC_API_KEY" opt-env)))
            (if (and (string? env-val) (not (string-null? env-val))) env-val #f)
          ) ;let
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-load-models-store agent-dir)
      (let* ((filepath (path->string (path-join (path agent-dir) (path "models-store.json")))
             ) ;filepath
             (data (pi-read-json-file filepath))
            ) ;
        (if (pair? data) data '())
      ) ;let*
    ) ;define

    (define (%find-model-in-list models model-id)
      (let loop
        ((lst models))
        (cond ((null? lst) #f)
              ((string=? (json-dict-ref (car lst) "id" "") model-id) (car lst))
              (else (loop (cdr lst)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%get-provider-models models-store provider)
      (let* ((p-obj (json-dict-ref models-store provider '()))
             (raw-models (json-dict-ref p-obj "models" '()))
            ) ;
        (cond ((vector? raw-models) (vector->list raw-models))
              ((list? raw-models) raw-models)
              (else '())
        ) ;cond
      ) ;let*
    ) ;define

    (define (%apply-meta-env-overrides base-url context-window max-tokens opt-env)
      (let* ((env-url (lookup-env-var "GF_PI_BASE_URL" opt-env))
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
             (def-url (if is-ant?
                        "https://api.anthropic.com"
                        "https://generativelanguage.googleapis.com/v1beta"
                      ) ;if
             ) ;def-url
             (def-cw (if is-ant? 200000 128000))
             (def-mt (if is-ant? 8192 16384))
             (models (%get-provider-models models-store p-str))
             (m (%find-model-in-list models model-id))
             (matched? (if m #t #f))
             (name (if m (json-dict-ref m "name" model-id) model-id))
             (base-url (if m (json-dict-ref m "baseUrl" def-url) def-url))
             (cw (if m (json-dict-ref m "contextWindow" def-cw) def-cw))
             (mt (if m (json-dict-ref m "maxTokens" def-mt) def-mt))
            ) ;
        (let-values (((f-url f-cw f-mt) (%apply-meta-env-overrides base-url cw mt opt-env)))
          (list (cons 'id model-id)
            (cons 'name name)
            (cons 'provider p-str)
            (cons 'base-url f-url)
            (cons 'context-window f-cw)
            (cons 'max-tokens f-mt)
            (cons 'matched? matched?)
          ) ;list
        ) ;let-values
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
