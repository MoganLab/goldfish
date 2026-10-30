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
    gf-get-config-dir pi-cfg-config-dir pi-read-json-file pi-load-settings
    pi-save-default-thinking-level! pi-save-hide-thinking-block!
    pi-save-default-model! pi-model-supports-thinking?
    pi-load-compaction-settings pi-resolve-proxy-url pi-load-auth
    pi-resolve-provider-key pi-load-models-store pi-load-models-json
    pi-merge-models-json-into-store pi-resolve-model-metadata resolve-pi-config
    resolve-goldpi-config resolve-active-tools pi-missing-credential-msg
    augment-pi-config
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

    ;; JSON 配置文件目录：settings.json / auth.json / models.json /
    ;; models-store.json / multikey.json / web-search.json 等所在目录。
    ;; 优先级：显式目录参数 > GF_CONFIG_DIR > PI_CODING_AGENT_DIR
    ;;         > ~/.gf/agent（存在时） > ~/.pi/agent
    ;; 返回绝对路径（path 对象）；会话目录（sessions/）与扩展目录（extensions/）
    ;; 仍归属 agent 目录（pi-get-agent-dir）。
    (define (gf-get-config-dir . opt-dir)
      (let ((custom (and (pair? opt-dir) (car opt-dir)))
            (env-gf (get-environment-variable "GF_CONFIG_DIR"))
            (env-pi (get-environment-variable "PI_CODING_AGENT_DIR"))
           ) ;
        (cond
         ((and (string? custom) (not (string-null? (string-trim-both custom))))
          (pi-abs-path custom)
         ) ;
         ((and (string? env-gf) (not (string-null? (string-trim-both env-gf))))
          (pi-abs-path env-gf)
         ) ;
         ((and (string? env-pi) (not (string-null? (string-trim-both env-pi))))
          (pi-abs-path env-pi)
         ) ;
         (else
           (let ((gf-agent (pi-abs-path "~/.gf/agent")))
             (if (path-exists? gf-agent) gf-agent (pi-abs-path "~/.pi/agent"))
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-read-json-file filepath)
      (and (path-exists? filepath)
        (catch #t (lambda () (string->json (path-read-text filepath))) (lambda args #f))
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

    (define (pi-load-settings config-dir)
      (let* ((filepath (path-join config-dir "settings.json"))
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
          (json-dict-ref obj "sessionDir" #f)
          ;; 与上游共用 settings.json：hideThinkingBlock 隔离为单行斜体标签
          (%normalize-bool (json-dict-ref obj "hideThinkingBlock" #f))
        ) ;%make-pi-settings
      ) ;let*
    ) ;define

    (define (pi-save-default-thinking-level! config-dir level)
      (let* ((filepath (path-join config-dir "settings.json"))
             (settings (or (pi-read-json-file filepath) '()))
             (updated
               (cons (cons "defaultThinkingLevel" level)
                 (filter
                   (lambda (entry)
                     (not (or (equal? (car entry) "defaultThinkingLevel")
                            (equal? (car entry) 'defaultThinkingLevel)
                          ) ;or
                     ) ;not
                   ) ;lambda
                   settings
                 ) ;filter
               ) ;cons
             ) ;updated
            ) ;
        (path-mkdir config-dir :parents #t :exist-ok #t)
        (path-write-text filepath (string-append (json->string updated) "\n"))
        level
      ) ;let*
    ) ;define

    ;; /model Ctrl+S 持久化：只更新 defaultProvider/defaultModel，保留其他设置
    (define (pi-save-default-model! config-dir provider model-id)
      (let* ((filepath (path-join config-dir "settings.json"))
             (settings (or (pi-read-json-file filepath) '()))
             (updated
               (append (list (cons "defaultProvider" provider) (cons "defaultModel" model-id))
                 (filter
                   (lambda (entry)
                     (not (or (equal? (car entry) "defaultProvider")
                            (equal? (car entry) 'defaultProvider)
                            (equal? (car entry) "defaultModel")
                            (equal? (car entry) 'defaultModel)
                          ) ;or
                     ) ;not
                   ) ;lambda
                   settings
                 ) ;filter
               ) ;append
             ) ;updated
            ) ;
        (path-mkdir config-dir :parents #t :exist-ok #t)
        (path-write-text filepath (string-append (json->string updated) "\n"))
        (list provider model-id)
      ) ;let*
    ) ;define

    ;; Ctrl+T 切换思考块显隐后持久化：只更新 hideThinkingBlock，保留其他设置
    ;; （对齐上游 setHideThinkingBlock；JSON 布尔值为符号 true/false）
    (define (pi-save-hide-thinking-block! config-dir hide)
      (let* ((filepath (path-join config-dir "settings.json"))
             (settings (or (pi-read-json-file filepath) '()))
             (updated
               (cons (cons "hideThinkingBlock" (if hide 'true 'false))
                 (filter
                   (lambda (entry)
                     (not (or (equal? (car entry) "hideThinkingBlock")
                            (equal? (car entry) 'hideThinkingBlock)
                          ) ;or
                     ) ;not
                   ) ;lambda
                   settings
                 ) ;filter
               ) ;cons
             ) ;updated
            ) ;
        (path-mkdir config-dir :parents #t :exist-ok #t)
        (path-write-text filepath (string-append (json->string updated) "\n"))
        (and hide #t)
      ) ;let*
    ) ;define

    (define (pi-load-compaction-settings config-dir)
      (let* ((filepath (path-join config-dir "settings.json"))
             (obj (or (pi-read-json-file filepath) '()))
             (defaults (pi-compaction-default-settings))
             (node (json-dict-ref obj "compaction" '()))
             (get (lambda (key default) (json-dict-ref node key default)))
            ) ;
        (make-pi-compaction-settings (%normalize-bool (get "enabled" (pi-compaction-settings-enabled defaults)))
          (get "reserveTokens" (pi-compaction-settings-reserve-tokens defaults))
          (get "keepRecentTokens" (pi-compaction-settings-keep-recent-tokens defaults))
        ) ;make-pi-compaction-settings
      ) ;let*
    ) ;define

    ;; 从 cfg 中取 JSON 配置文件目录（path 对象）：
    ;; 优先 config-dir，缺失时回退 agent-dir
    (define (pi-cfg-config-dir cfg)
      (if (pair? cfg)
        (cdr (or (assoc 'config-dir cfg) (assoc 'agent-dir cfg) '(#f . #f)))
        #f
      ) ;if
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
        (fold-right (lambda (name acc) (if (member name acc) acc (cons name acc)))
          '()
          (append PI_DEFAULT_TOOL_NAMES ext-names)
        ) ;fold-right
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
      (let ((model
              (find (lambda (entry) (equal? (json-dict-ref entry "id" #f) model-id))
                (pi-catalog-provider-models models-store provider)
              ) ;find
            ) ;model
           ) ;
        (if model (and (json-dict-ref model "reasoning" #f) #t) #t)
      ) ;let
    ) ;define

    ;; 公开包装：/model 切换后重算 supports-thinking（worker 与 TUI 共用）
    (define (pi-model-supports-thinking? models-store provider model-id)
      (%model-supports-thinking? models-store provider model-id)
    ) ;define

    (define (resolve-pi-config parsed-args . opt-env)
      (let* ((env-agent-dir (lookup-env-var "PI_CODING_AGENT_DIR" opt-env))
             (agent-dir (pi-get-agent-dir env-agent-dir))
             ;; JSON 配置文件目录：~/.gf/agent 存在时优先，否则回退 ~/.pi/agent
             (config-dir (gf-get-config-dir env-agent-dir))
             (settings (pi-load-settings config-dir))
             (models-store (pi-build-models-catalog config-dir))
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
                         (pi-load-auth config-dir (car opt-env))
                         (pi-load-auth config-dir)
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
              (cons 'config-dir config-dir)
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
