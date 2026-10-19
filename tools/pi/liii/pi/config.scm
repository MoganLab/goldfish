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
    (liii pi args)
    (liii pi tools)
    (liii pi auth)
    (liii pi models)
    (liii pi config types)
    (liii pi config record)
    (liii pi event)
  ) ;import
  (export pi-resolve-config-value json-dict-ref pi-get-agent-dir
    pi-read-json-file pi-load-settings pi-resolve-proxy-url pi-load-auth
    pi-resolve-provider-key pi-load-models-store pi-resolve-model-metadata
    resolve-pi-config resolve-goldpi-config resolve-active-tools
    pi-warn-fallback-model goldpi-warn-fallback-model
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
      (let ((custom (and (pair? opt-dir) (car opt-dir)))
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
      (and (path-exists? (path filepath))
        (catch #t
          (lambda () (string->json (path-read-text (path filepath))))
          (lambda args #f)
        ) ;catch
      ) ;and
    ) ;define

    (define (%normalize-bool val)
      (case val
       ((true) #t)
       ((false) #f)
       (else val)
      ) ;case
    ) ;define

    (define (%parse-tools-list val)
      (cond ((vector? val) (vector->list val))
            ((list? val) val)
            (else #f)
      ) ;cond
    ) ;define

    (define (%build-retry-record retry-obj)
      (%make-pi-retry-config (%normalize-bool (json-dict-ref retry-obj "enabled" #t))
        (json-dict-ref retry-obj "maxRetries" 3)
        (json-dict-ref retry-obj "baseDelayMs" 2000)
        (json-dict-ref retry-obj "maxAgentDelayMs" 60000)
      ) ;%make-pi-retry-config
    ) ;define

    (define (pi-load-settings agent-dir)
      (let* ((filepath (path->string (path-join (path agent-dir) (path "settings.json"))))
             (obj (or (pi-read-json-file filepath) '()))
             (raw-proxy (json-dict-ref obj "httpProxy" #f))
             (clean-proxy
               (and (string? raw-proxy)
                 (not (string-null? (string-trim-both raw-proxy)))
                 raw-proxy
               ) ;and
             ) ;clean-proxy
            ) ;
        (%make-pi-settings (json-dict-ref obj "defaultProvider" "google")
          (json-dict-ref obj "defaultModel" "gemini-3.8-flash")
          (json-dict-ref obj "defaultThinkingLevel" "off")
          clean-proxy
          (%build-retry-record (json-dict-ref obj "retry" '()))
          (%parse-tools-list (json-dict-ref obj "defaultTools" #f))
        ) ;%make-pi-settings
      ) ;let*
    ) ;define

    (define (pi-resolve-proxy-url cli-proxy settings . opt-env)
      (let ((env-https (lookup-env-var "HTTPS_PROXY" opt-env))
            (env-all (lookup-env-var "ALL_PROXY" opt-env))
            (sp (and (assoc 'http-proxy settings) (cdr (assoc 'http-proxy settings))))
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
         ((and (string? sp) (not (string-null? (string-trim-both sp)))) sp)
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (resolve-active-tools parsed-args settings)
      (if (pi-args-ref parsed-args 'no-tools)
        '()
        (let* ((cli-tools (pi-args-ref parsed-args 'tools))
               (cli-exclude (pi-args-ref parsed-args 'exclude-tools))
               (settings-tools (cdr (assoc 'default-tools settings)))
               (base-tools
                 (cond
                  ((and (string? cli-tools) (not (string-null? (string-trim-both cli-tools))))
                   (parse-tools-selection cli-tools)
                  ) ;
                  ((pair? settings-tools)
                   (if
                     (and (string? (car settings-tools))
                       (or (string-starts? (car settings-tools) "+")
                         (string-starts? (car settings-tools) "-")
                       ) ;or
                     ) ;and
                     (parse-tools-selection (string-join settings-tools ","))
                     settings-tools
                   ) ;if
                  ) ;
                  (else PI_DEFAULT_TOOL_NAMES)
                 ) ;cond
               ) ;base-tools
              ) ;
          (if
            (and (string? cli-exclude) (not (string-null? (string-trim-both cli-exclude))))
            (exclude-tools-from-list base-tools cli-exclude)
            base-tools
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-warn-fallback-model model-id)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display "Warning: Model \"" p)
        (display model-id p)
        (display "\" not found in models store; using fallback defaults\n" p)
      ) ;let
    ) ;define
    (define goldpi-warn-fallback-model pi-warn-fallback-model)

    (define (resolve-pi-config parsed-args . opt-env)
      (let* ((env-agent-dir (lookup-env-var "PI_CODING_AGENT_DIR" opt-env))
             (agent-dir (pi-get-agent-dir env-agent-dir))
             (settings (pi-load-settings agent-dir))
             (models-store (pi-load-models-store agent-dir))
             (cli-proxy (pi-args-ref parsed-args 'proxy))
             (proxy-url (if (pair? opt-env)
                          (pi-resolve-proxy-url cli-proxy settings (car opt-env))
                          (pi-resolve-proxy-url cli-proxy settings)
                        ) ;if
             ) ;proxy-url
             (tools (resolve-active-tools parsed-args settings))
            ) ;
        (let-values (((provider model-id)
                      (pi-route-provider-and-model (pi-args-ref parsed-args 'provider)
                        (pi-args-ref parsed-args 'model)
                        settings
                        models-store
                        opt-env
                      ) ;pi-route-provider-and-model
                     ) ;
                    ) ;
          (let* ((auth (if (pair? opt-env)
                         (pi-load-auth agent-dir (car opt-env))
                         (pi-load-auth agent-dir)
                       ) ;if
                 ) ;auth
                 (api-key (if (pair? opt-env)
                            (pi-resolve-provider-key provider auth (car opt-env))
                            (pi-resolve-provider-key provider auth)
                          ) ;if
                 ) ;api-key
                 (raw-meta (if (pair? opt-env)
                             (pi-resolve-model-metadata provider model-id models-store (car opt-env))
                             (pi-resolve-model-metadata provider model-id models-store)
                           ) ;if
                 ) ;raw-meta
                 (meta (pi-ensure-catalog-meta raw-meta provider model-id models-store))
                 (cli-thinking (pi-args-ref parsed-args 'thinking-level))
                 (settings-thinking (and settings (pi-settings-default-thinking-level settings)))
                 (thinking-level
                   (cond
                    ((and (string? cli-thinking)
                       (not (string-null? (string-trim-both cli-thinking)))
                     ) ;and
                     cli-thinking
                    ) ;
                    ((and (string? settings-thinking)
                       (not (string-null? (string-trim-both settings-thinking)))
                     ) ;and
                     settings-thinking
                    ) ;
                    (else "off")
                   ) ;cond
                 ) ;thinking-level
                ) ;
            (when (not (pi-model-metadata-matched? meta))
              (pi-warn-fallback-model model-id)
            ) ;when
            (list (cons 'agent-dir agent-dir)
              (cons 'provider provider)
              (cons 'api-key api-key)
              (cons 'model-id model-id)
              (cons 'model-meta meta)
              (cons 'models-store models-store)
              (cons 'thinking-level thinking-level)
              (cons 'proxy-url proxy-url)
              (cons 'tools tools)
              (cons 'settings settings)
            ) ;list
          ) ;let*
        ) ;let-values
      ) ;let*
    ) ;define
    (define resolve-goldpi-config resolve-pi-config)

  ) ;begin
) ;define-library
