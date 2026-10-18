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

(define-library (liii goldpi-config)
  (import (scheme base)
    (scheme process-context)
    (scheme write)
    (liii string)
    (liii pi)
    (liii pi template)
    (liii goldpi-args)
    (liii goldpi-tools)
  ) ;import
  (export resolve-goldpi-config resolve-active-tools goldpi-warn-fallback-model)
  (begin

    (define (%resolve-model-id parsed-args settings opt-env provider)
      (let* ((cli-model (pi-args-ref parsed-args 'model))
             (env-model (or (lookup-env-var "GF_PI_MODEL" opt-env)
                          (if (equal? provider "anthropic")
                            (lookup-env-var "ANTHROPIC_DEFAULT_SONNET_MODEL" opt-env)
                            #f
                          ) ;if
                        ) ;or
             ) ;env-model
             (settings-model (cdr (assoc 'default-model settings)))
             (settings-provider (cdr (assoc 'default-provider settings)))
             (def-model (if (equal? provider "anthropic")
                          "claude-3-7-sonnet-20250219"
                          "gemini-3.8-flash"
                        ) ;if
             ) ;def-model
            ) ;
        (cond
         ((and (string? cli-model) (not (string-null? (string-trim-both cli-model))))
          cli-model
         ) ;
         ((and (string? env-model) (not (string-null? (string-trim-both env-model))))
          env-model
         ) ;
         ((and (equal? provider settings-provider)
            (string? settings-model)
            (not (string-null? (string-trim-both settings-model)))
          ) ;and
          settings-model
         ) ;
         (else def-model)
        ) ;cond
      ) ;let*
    ) ;define

    (define (%resolve-provider parsed-args settings)
      (let ((cli-provider (pi-args-ref parsed-args 'provider))
            (settings-provider (cdr (assoc 'default-provider settings)))
           ) ;
        (cond
         ((and (string? cli-provider)
            (not (string-null? (string-trim-both cli-provider)))
          ) ;and
          cli-provider
         ) ;
         ((and (string? settings-provider)
            (not (string-null? (string-trim-both settings-provider)))
          ) ;and
          settings-provider
         ) ;
         (else "google")
        ) ;cond
      ) ;let
    ) ;define

    (define (resolve-active-tools parsed-args settings)
      (let ((cli-tools (pi-args-ref parsed-args 'tools))
            (settings-tools (cdr (assoc 'default-tools settings)))
           ) ;
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
      ) ;let
    ) ;define

    (define (goldpi-warn-fallback-model model-id)
      (display "Warning: Model \"" (current-error-port))
      (display model-id (current-error-port))
      (display "\" not found in models store; using fallback defaults\n"
        (current-error-port)
      ) ;display
    ) ;define

    (define (resolve-goldpi-config parsed-args . opt-env)
      (let* ((env-agent-dir (lookup-env-var "PI_CODING_AGENT_DIR" opt-env))
             (agent-dir (pi-get-agent-dir env-agent-dir))
             (settings (pi-load-settings agent-dir))
             (provider (%resolve-provider parsed-args settings))
             (auth (if (pair? opt-env)
                     (pi-load-auth agent-dir (car opt-env))
                     (pi-load-auth agent-dir)
                   ) ;if
             ) ;auth
             (api-key (if (pair? opt-env)
                        (pi-resolve-provider-key provider auth (car opt-env))
                        (pi-resolve-provider-key provider auth)
                      ) ;if
             ) ;api-key
             (model-id (%resolve-model-id parsed-args settings opt-env provider))
             (models-store (pi-load-models-store agent-dir))
             (meta (if (pair? opt-env)
                     (pi-resolve-model-metadata provider model-id models-store (car opt-env))
                     (pi-resolve-model-metadata provider model-id models-store)
                   ) ;if
             ) ;meta
             (cli-proxy (pi-args-ref parsed-args 'proxy))
             (proxy-url (if (pair? opt-env)
                          (pi-resolve-proxy-url cli-proxy settings (car opt-env))
                          (pi-resolve-proxy-url cli-proxy settings)
                        ) ;if
             ) ;proxy-url
             (tools (resolve-active-tools parsed-args settings))
            ) ;
        (list (cons 'agent-dir agent-dir)
          (cons 'provider provider)
          (cons 'api-key api-key)
          (cons 'model-id model-id)
          (cons 'model-meta meta)
          (cons 'proxy-url proxy-url)
          (cons 'tools tools)
          (cons 'settings settings)
        ) ;list
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
