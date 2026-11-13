;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai auth)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii path)
    (liii json)
    (liii list)
    (liii error)
    (liii os)
    (liii pi ai template)
    (liii pi ai config types)
    (liii pi ai config record)
    (liii pi ai event)
  ) ;import
  (export pi-load-auth pi-resolve-provider-key pi-save-auth-key)
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

    (define (pi-load-auth config-dir . opt-env)
      (let* ((filepath (path-join config-dir "auth.json"))
             (data (%read-json-file filepath))
            ) ;
        (if (not (pair? data))
          '()
          (filter (lambda (credential) credential)
            (map (lambda (entry) (%parse-credential-item (car entry) (cdr entry) opt-env))
              data
            ) ;map
          ) ;filter
        ) ;if
      ) ;let*
    ) ;define

    (define (%find-claude-deepseek-token home)
      (if (string-null? home)
        #f
        (let* ((f (path->string (path-join (path home) (path ".claude") (path "settings.json.deepseek"))
                  ) ;path->string
               ) ;f
               (data (%read-json-file f))
              ) ;
          (and data
            (let ((e (%dict-ref data "env" #f)))
              (and (pair? e)
                (let ((tok (%dict-ref e "ANTHROPIC_AUTH_TOKEN" #f)))
                  (and (string? tok) (not (string-null? tok)) tok)
                ) ;let
              ) ;and
            ) ;let
          ) ;and
        ) ;let*
      ) ;if
    ) ;define

    (define (%find-dot-deepseek-key home)
      (if (string-null? home)
        #f
        (let* ((f (path->string (path-join (path home) (path ".deepseek.json"))))
               (data (%read-json-file f))
              ) ;
          (and data
            (let ((k (%dict-ref data "api_key" #f)))
              (and (string? k) (not (string-null? k)) k)
            ) ;let
          ) ;and
        ) ;let*
      ) ;if
    ) ;define

    (define (%env-var-non-empty name opt-env)
      (let ((v (lookup-env-var name opt-env)))
        (and (string? v) (not (string-null? (string-trim-both v))) v)
      ) ;let
    ) ;define

    (define (%resolve-deepseek-fallback opt-env)
      (let* ((home (or (lookup-env-var "HOME" opt-env) ""))
             (claude-k (%find-claude-deepseek-token home))
            ) ;
        (if claude-k
          claude-k
          (let ((ds-k (%find-dot-deepseek-key home)))
            (if ds-k ds-k (%env-var-non-empty "ANTHROPIC_AUTH_TOKEN" opt-env))
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (%resolve-provider-env-key p-str opt-env)
      (cond ((string=? p-str "google") (%env-var-non-empty "GEMINI_API_KEY" opt-env))
            ((string=? p-str "kimi-coding") (%env-var-non-empty "KIMI_API_KEY" opt-env))
            ((string=? p-str "anthropic")
             (or (%env-var-non-empty "ANTHROPIC_API_KEY" opt-env)
               (%env-var-non-empty "ANTHROPIC_AUTH_TOKEN" opt-env)
             ) ;or
            ) ;
            ((string=? p-str "glm")
             (or (%env-var-non-empty "ANTHROPIC_AUTH_TOKEN" opt-env)
               (%env-var-non-empty "GLM_API_KEY" opt-env)
               (%env-var-non-empty "ANTHROPIC_API_KEY" opt-env)
             ) ;or
            ) ;
            ((string=? p-str "deepseek") (%env-var-non-empty "DEEPSEEK_API_KEY" opt-env))
            ((string=? p-str "ollama") (%env-var-non-empty "OLLAMA_API_KEY" opt-env))
            ((string=? p-str "minimax-cn")
             (or (%env-var-non-empty "MINIMAX_CN_API_KEY" opt-env)
               (%env-var-non-empty "MINIMAX_API_KEY" opt-env)
             ) ;or
            ) ;
            ((or (string=? p-str "zai-coding-cn") (string=? p-str "zai"))
             (or (%env-var-non-empty "ZAI_CODING_CN_API_KEY" opt-env)
               (%env-var-non-empty "ZAI_API_KEY" opt-env)
               (%env-var-non-empty "GLM_API_KEY" opt-env)
               (%env-var-non-empty "ZHIPU_API_KEY" opt-env)
             ) ;or
            ) ;
            ((or (string=? p-str "opencode") (string=? p-str "opencode-go"))
             (or (%env-var-non-empty "OPENCODE_API_KEY" opt-env)
               (%env-var-non-empty "OPENCODE_GO_API_KEY" opt-env)
             ) ;or
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (%provider-alias p-str)
      (cond ((string=? p-str "zai-coding-cn") "zai")
            ((string=? p-str "zai") "zai-coding-cn")
            ((string=? p-str "opencode") "opencode-go")
            ((string=? p-str "opencode-go") "opencode")
            (else #f)
      ) ;cond
    ) ;define

    (define (%non-empty-auth-key pair)
      (and pair
        (string? (cdr pair))
        (not (string-null? (string-trim-both (cdr pair))))
        (cdr pair)
      ) ;and
    ) ;define

    (define (pi-resolve-provider-key provider auth-alist . opt-env)
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (env-k (%resolve-provider-env-key p-str opt-env))
            ) ;
        (if env-k
          env-k
          (let* ((pair (or (assoc p-str auth-alist) (assoc (string->symbol p-str) auth-alist)))
                 (alias (%provider-alias p-str))
                 (alias-pair
                   (and alias
                     (or (assoc alias auth-alist) (assoc (string->symbol alias) auth-alist))
                   ) ;and
                 ) ;alias-pair
                ) ;
            (or (%non-empty-auth-key pair)
              (%non-empty-auth-key alias-pair)
              (cond ((string=? p-str "deepseek") (%resolve-deepseek-fallback opt-env))
                    ((string=? p-str "ollama") "ollama")
                    (else #f)
              ) ;cond
            ) ;or
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%normalize-provider-key provider)
      (cond
       ((string? provider)
        (let ((trimmed (string-trim-both provider)))
          (if (string-null? trimmed)
            (value-error "pi-save-auth-key: provider cannot be empty")
            trimmed
          ) ;if
        ) ;let
       ) ;
       ((symbol? provider)
        (let ((trimmed (string-trim-both (symbol->string provider))))
          (if (string-null? trimmed)
            (value-error "pi-save-auth-key: provider cannot be empty")
            trimmed
          ) ;if
        ) ;let
       ) ;
       (else (type-error "pi-save-auth-key: provider must be a string or symbol"))
      ) ;cond
    ) ;define

    (define (%normalize-auth-key key)
      (if (not (string? key))
        (type-error "pi-save-auth-key: key must be a string")
        (let ((trimmed (string-trim-both key)))
          (if (string-null? trimmed)
            (value-error "pi-save-auth-key: key cannot be empty")
            trimmed
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%update-auth-alist alist p-str key)
      (let* ((filtered
               (filter
                 (lambda (entry)
                   (not
                     (string=? (if (symbol? (car entry)) (symbol->string (car entry)) (car entry))
                       p-str
                     ) ;string=?
                   ) ;not
                 ) ;lambda
                 alist
               ) ;filter
             ) ;filtered
             (new-entry (cons p-str (list (cons "type" "api_key") (cons "key" key))))
            ) ;
        (append filtered (list new-entry))
      ) ;let*
    ) ;define

    (define (pi-save-auth-key provider key . opt-config-dir)
      (let* ((p-str (%normalize-provider-key provider))
             (k-str (%normalize-auth-key key))
             (dir-path
               (path (if (and (pair? opt-config-dir) (car opt-config-dir))
                       (car opt-config-dir)
                       (path-join (path-home) ".gf" "agent")
                     ) ;if
               ) ;path
             ) ;dir-path
             (auth-file (path-join dir-path "auth.json"))
             (auth-file-str (path->string auth-file))
             (existing (or (%read-json-file auth-file-str) '()))
             (existing-alist (if (pair? existing) existing '()))
             (updated (%update-auth-alist existing-alist p-str k-str))
             (json-str (string-append (json->string updated :indent 2) "\n"))
             (tmp-file (string-append auth-file-str ".tmp"))
            ) ;
        (unless (path-exists? dir-path)
          (path-mkdir dir-path :parents #t :exist-ok #t)
        ) ;unless
        (path-write-text (path tmp-file) json-str)
        (when (path-exists? auth-file)
          (path-unlink auth-file #t)
        ) ;when
        (path-rename (path tmp-file) auth-file)
        (unless (os-windows?)
          (catch #t (lambda () (g_chmod auth-file-str 384)) (lambda args #f))
        ) ;unless
        auth-file-str
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
