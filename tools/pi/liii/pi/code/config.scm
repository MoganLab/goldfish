;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code config)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii list)
    (liii path)
    (liii os)
    (liii json)
    (liii error)
    (liii pi code core tools read)
    (liii pi ai template)
    (liii pi code cli args)
    (liii pi code core tools)
    (liii pi ai auth)
    (liii pi ai models)
    (liii pi ai config types)
    (liii pi ai config record)
    (liii pi ai event)
    (liii pi code core extensions registry)
    (liii pi code core extensions event)
    (liii pi code core compaction tokens)
  ) ;import
  (export pi-resolve-config-value json-dict-ref pi-get-agent-dir
    pi-read-json-file pi-load-settings pi-load-compaction-settings
    pi-resolve-proxy-url pi-load-auth pi-resolve-provider-key
    pi-load-models-store pi-load-models-json pi-merge-models-json-into-store
    pi-resolve-model-metadata resolve-pi-config resolve-goldpi-config
    resolve-active-tools pi-missing-credential-msg augment-pi-config
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

    ;; pi-get-agent-dir 已下沉至 (liii pi code core tools read)，此处经 import 再导出

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
      (let ((provider-obj (json-dict-ref retry-obj "provider" '())))
        (%make-pi-retry-config (%normalize-bool (json-dict-ref retry-obj "enabled" #t))
          (json-dict-ref retry-obj "maxRetries" 3)
          (json-dict-ref retry-obj "baseDelayMs" 2000)
          (json-dict-ref retry-obj "maxAgentDelayMs" 60000)
          (json-dict-ref provider-obj "maxRetries" #f)
        ) ;%make-pi-retry-config
      ) ;let
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

    (define (pi-load-compaction-settings agent-dir)
      (let* ((filepath (path->string (path-join (path agent-dir) (path "settings.json"))))
             (obj (or (pi-read-json-file filepath) '()))
             (defaults (pi-compaction-default-settings))
             (node (json-dict-ref obj "compaction" '()))
             (get (lambda (key default) (json-dict-ref node key default)))
            ) ;
        (list
          (cons 'enabled
            (%normalize-bool (get "enabled" (cdr (assoc 'enabled defaults))))
          ) ;cons
          (cons 'reserve-tokens
            (get "reserveTokens" (cdr (assoc 'reserve-tokens defaults)))
          ) ;cons
          (cons 'keep-recent-tokens
            (get "keepRecentTokens" (cdr (assoc 'keep-recent-tokens defaults)))
          ) ;cons
        ) ;list
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

    ;; 有效默认基础集 = 内置默认 + 注册表中声明 :default-active? 的扩展工具
    ;; （去重、保持顺序；reg 缺省动态取当前/默认注册表，传 #f 同样动态取）
    (define (%effective-base-tools reg)
      (let* ((r (or reg (current-extension-registry) (default-extension-registry)))
             (ext-names (if r (extension-registry-default-tool-names r) '()))
            ) ;
        (let loop
          ((rest (append PI_DEFAULT_TOOL_NAMES ext-names)) (seen '()) (acc '()))
          (if (null? rest)
            (reverse acc)
            (let ((n (car rest)))
              (if (member n seen)
                (loop (cdr rest) seen acc)
                (loop (cdr rest) (cons n seen) (cons n acc))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define* (resolve-active-tools parsed-args settings (reg #f))
      (if (pi-args-ref parsed-args 'no-tools)
        '()
        (let* ((cli-tools (pi-args-ref parsed-args 'tools))
               (cli-exclude (pi-args-ref parsed-args 'exclude-tools))
               (settings-tools (cdr (assoc 'default-tools settings)))
               (base-tools (%effective-base-tools reg))
               (base-tools
                 (cond
                  ((and (string? cli-tools) (not (string-null? (string-trim-both cli-tools))))
                   (parse-tools-selection cli-tools base-tools)
                  ) ;
                  ((pair? settings-tools)
                   (if
                     (and (string? (car settings-tools))
                       (or (string-starts? (car settings-tools) "+")
                         (string-starts? (car settings-tools) "-")
                       ) ;or
                     ) ;and
                     (parse-tools-selection (string-join settings-tools ",") base-tools)
                     settings-tools
                   ) ;if
                  ) ;
                  (else base-tools)
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
    ) ;define*

    (define (%make-extension-emitter)
      ;; 动态取当前注册表：与 with-extension-registry 切换语义保持一致
      (lambda (ev-type payload)
        (let ((reg (or (current-extension-registry) (default-extension-registry))))
          (pi-emit! reg ev-type payload)
        ) ;let
      ) ;lambda
    ) ;define

    ;; 从 models-store 原始条目查询模型是否支持 reasoning；
    ;; 元数据缺失时默认支持（对齐上游：无模型信息时返回全集级别）
    (define (%model-supports-thinking? models-store provider model-id)
      (let loop
        ((rest (pi-catalog-provider-models models-store provider)))
        (cond ((null? rest) #t)
              ((equal? (json-dict-ref (car rest) "id" #f) model-id)
               (and (json-dict-ref (car rest) "reasoning" #f) #t)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (resolve-pi-config parsed-args . opt-env)
      (let* ((env-agent-dir (lookup-env-var "PI_CODING_AGENT_DIR" opt-env))
             (agent-dir (pi-get-agent-dir env-agent-dir))
             (settings (pi-load-settings agent-dir))
             (models-store (pi-build-models-catalog agent-dir))
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
                 (api-key-raw (if (pair? opt-env)
                                (pi-resolve-provider-key provider auth (car opt-env))
                                (pi-resolve-provider-key provider auth)
                              ) ;if
                 ) ;api-key-raw
                 (api-key api-key-raw)
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
                 (supports-thinking (%model-supports-thinking? models-store provider model-id))
                ) ;
            (list (cons 'agent-dir agent-dir)
              (cons 'provider provider)
              (cons 'api-key api-key)
              (cons 'model-id model-id)
              (cons 'model-meta meta)
              (cons 'models-store models-store)
              (cons 'thinking-level thinking-level)
              (cons 'supports-thinking supports-thinking)
              (cons 'proxy-url proxy-url)
              (cons 'tools tools)
              (cons 'tool-resolver get-tool-definition)
              (cons 'execute-tool-call execute-tool-call)
              (cons 'emit-extension-event (%make-extension-emitter))
              (cons 'settings settings)
            ) ;list
          ) ;let*
        ) ;let-values
      ) ;let*
    ) ;define
    (define resolve-goldpi-config resolve-pi-config)

    ;; 应用扩展注册的 config augmenter：multikey 等内置/用户扩展经此
    ;; 注入 request-key / rotate-key 等钩子（CLI dispatch 与 TUI worker 共用）
    (define (augment-pi-config cfg parsed)
      (let* ((reg (or (current-extension-registry) (default-extension-registry))))
        (fold (lambda (aug c) (aug c parsed)) cfg (extension-registry-augmenters reg))
      ) ;let*
    ) ;define

    ;; 凭证缺失时返回用户可读提示消息，否则返回 #f（CLI 报错与 TUI 卡片共用）
    (define (pi-missing-credential-msg cfg)
      (let ((provider (cdr (or (assoc 'provider cfg) '(#f . "google"))))
            (api-key (cdr (or (assoc 'api-key cfg) '(#f . #f))))
           ) ;
        (if (or (not api-key) (and (string? api-key) (string-null? api-key)))
          (string-append "No credentials found for provider \"" provider "\"")
          #f
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
