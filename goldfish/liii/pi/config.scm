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
  ) ;import
  (export pi-resolve-config-value json-dict-ref pi-get-agent-dir
    pi-read-json-file pi-load-settings pi-resolve-proxy-url
  ) ;export
  (begin

    (define (env-name-start-char? ch)
      (or (and (char>=? ch #\a) (char<=? ch #\z))
        (and (char>=? ch #\A) (char<=? ch #\Z))
        (char=? ch #\_)
      ) ;or
    ) ;define

    (define (env-name-body-char? ch)
      (or (env-name-start-char? ch) (and (char>=? ch #\0) (char<=? ch #\9)))
    ) ;define

    (define (valid-env-name? str)
      (let ((len (string-length str)))
        (and (> len 0)
          (env-name-start-char? (string-ref str 0))
          (let loop
            ((i 1))
            (cond ((= i len) #t)
                  ((not (env-name-body-char? (string-ref str i))) #f)
                  (else (loop (+ i 1)))
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (lookup-env-var name custom-env)
      (cond
       ((and (pair? custom-env) (assoc name (car custom-env)))
        (cdr (assoc name (car custom-env)))
       ) ;
       (else (get-environment-variable name))
      ) ;cond
    ) ;define

    (define (extract-env-var-name str start len)
      (let loop
        ((i start))
        (cond
         ((and (< i len) (env-name-body-char? (string-ref str i))) (loop (+ i 1)))
         (else (values (substring str start i) i))
        ) ;cond
      ) ;let
    ) ;define

    (define (resolve-braced-var str i len custom-env parts)
      (let ((brace-end (string-index str #\} (+ i 2))))
        (if (not brace-end)
          (values (+ i 1) (cons "$" parts))
          (let ((name (substring str (+ i 2) brace-end)))
            (if (valid-env-name? name)
              (let ((val (lookup-env-var name custom-env)))
                (if (not val) (values -1 #f) (values (+ brace-end 1) (cons val parts)))
              ) ;let
              (values (+ brace-end 1) (cons (substring str i (+ brace-end 1)) parts))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (resolve-simple-var str i len custom-env parts)
      (let-values (((name next-idx) (extract-env-var-name str (+ i 1) len)))
        (let ((val (lookup-env-var name custom-env)))
          (if (not val) (values -1 #f) (values next-idx (cons val parts)))
        ) ;let
      ) ;let-values
    ) ;define

    (define (process-dollar-token str i len custom-env parts)
      (let ((c1 (string-ref str (+ i 1))))
        (cond
         ((or (char=? c1 #\$) (char=? c1 #\!)) (values (+ i 2) (cons (string c1) parts)))
         ((char=? c1 #\{) (resolve-braced-var str i len custom-env parts))
         ((env-name-start-char? c1) (resolve-simple-var str i len custom-env parts))
         (else (values (+ i 1) (cons "$" parts)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%advance-dollar str dollar-pos len custom-env parts-with-prefix)
      (if (>= (+ dollar-pos 1) len)
        (values -2 (cons "$" parts-with-prefix))
        (process-dollar-token str dollar-pos len custom-env parts-with-prefix)
      ) ;if
    ) ;define

    (define (%loop-template-parts str len custom-env)
      (let loop
        ((i 0) (parts '()))
        (cond ((>= i len) (apply string-append (reverse parts)))
              (else
                (let ((dollar-pos (string-index str #\$ i)))
                  (if (not dollar-pos)
                    (apply string-append (reverse (cons (substring str i len) parts)))
                    (let* ((prefix (substring str i dollar-pos))
                           (parts1 (if (> (string-length prefix) 0) (cons prefix parts) parts))
                          ) ;
                      (let-values (((next-i next-parts) (%advance-dollar str dollar-pos len custom-env parts1)))
                        (cond ((= next-i -1) #f)
                              ((= next-i -2) (apply string-append (reverse next-parts)))
                              (else (loop next-i next-parts))
                        ) ;cond
                      ) ;let-values
                    ) ;let*
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-resolve-config-value str . custom-env)
      (when (not (string? str))
        (type-error "pi-resolve-config-value: expected string")
      ) ;when
      (if (string-starts? str "!")
        (error 'pi-config-error "command syntax not supported in config value" str)
        (%loop-template-parts str (string-length str) custom-env)
      ) ;if
    ) ;define

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
            ((boolean? val) val)
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
      (let* ((get-env
               (lambda (name)
                 (if (pair? opt-env)
                   (let ((pair (assoc name (car opt-env))))
                     (if pair (cdr pair) #f)
                   ) ;let
                   (get-environment-variable name)
                 ) ;if
               ) ;lambda
             ) ;get-env
             (env-https (get-env "HTTPS_PROXY"))
             (env-all (get-env "ALL_PROXY"))
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
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
