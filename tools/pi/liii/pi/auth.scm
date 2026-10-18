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

(define-library (liii pi auth)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii path)
    (liii json)
    (liii pi template)
    (liii pi config types)
    (liii pi config record)
    (liii pi event)
  ) ;import
  (export pi-load-auth pi-resolve-provider-key)
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

    (define (%read-json-file filepath)
      (and (path-exists? (path filepath))
        (catch #t
          (lambda () (string->json (path-read-text (path filepath))))
          (lambda args #f)
        ) ;catch
      ) ;and
    ) ;define

    (define (%parse-credential-item key val opt-env)
      (and (pair? val)
        (equal? (%dict-ref val "type" #f) "api_key")
        (let ((raw-key (%dict-ref val "key" #f)))
          (and (string? raw-key)
            (let ((resolved (if (pair? opt-env)
                              (pi-resolve-config-value raw-key (car opt-env))
                              (pi-resolve-config-value raw-key)
                            ) ;if
                  ) ;resolved
                 ) ;
              (and resolved
                (%make-pi-credential (if (symbol? key) (symbol->string key) key)
                  resolved
                  "api_key"
                ) ;%make-pi-credential
              ) ;and
            ) ;let
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (pi-load-auth agent-dir . opt-env)
      (let* ((filepath (path->string (path-join (path agent-dir) (path "auth.json"))))
             (data (%read-json-file filepath))
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
          (let ((v (lookup-env-var "GEMINI_API_KEY" opt-env)))
            (and (string? v) (not (string-null? v)) v)
          ) ;let
         ) ;
         ((string=? p-str "anthropic")
          (let ((v (or (lookup-env-var "ANTHROPIC_API_KEY" opt-env)
                     (lookup-env-var "ANTHROPIC_AUTH_TOKEN" opt-env)
                   ) ;or
                ) ;v
               ) ;
            (and (string? v) (not (string-null? v)) v)
          ) ;let
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
