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
    (liii error)
    (liii pi code core tools read)
    (liii pi ai template)
    (liii pi code cli args)
    (liii pi code core tools)
    (liii pi ai auth)
    (liii pi ai models)
    (liii pi ai thinking)
    (liii pi ai providers registry)
    (liii pi ai config types)
    (liii pi ai config record)
    (liii pi ai event)
    (liii pi code config settings)
    (liii pi code config user-catalog)
    (liii pi code core extensions registry)
    (liii pi code core extensions event)
  ) ;import
  (export pi-resolve-config-value json-dict-ref pi-get-agent-dir
    gf-get-config-dir pi-cfg-config-dir pi-read-json-file pi-load-settings
    pi-save-default-thinking-level! pi-save-hide-thinking-block!
    pi-save-default-model! pi-model-supports-thinking? pi-cfg-provider-proxy
    pi-model-available-thinking-levels pi-cfg-provider-api
    pi-load-compaction-settings pi-resolve-proxy-url pi-load-auth
    pi-resolve-provider-key pi-resolve-model-metadata resolve-pi-config
    resolve-goldpi-config resolve-active-tools pi-missing-credential-msg
    augment-pi-config
  ) ;export
  (begin

    ;; pi-get-agent-dir 已下沉至 (liii pi code core tools read)，此处经 import 再导出

    ;; JSON 配置文件目录：settings.json / auth.json / models/providers.json /
    ;; models/（provider 与模型配置）/ models-store.json / multikey.json /
    ;; web-search.json 等所在目录。
    ;; 优先级：显式目录参数 > GF_CONFIG_DIR > PI_CODING_AGENT_DIR
    ;;         > ~/.gf/agent
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
         (else (pi-abs-path "~/.gf/agent"))
        ) ;cond
      ) ;let
    ) ;define

    ;; 从 cfg 中取 JSON 配置文件目录（path 对象）：
    ;; 优先 config-dir，缺失时回退 agent-dir
    (define (pi-cfg-config-dir cfg)
      (if (pair? cfg)
        (cdr (or (assoc 'config-dir cfg) (assoc 'agent-dir cfg) '(#f . #f)))
        #f
      ) ;if
    ) ;define

    ;; 从用户配置目录取 provider 层声明的 httpProxy（models/<provider>.json）
    (define (pi-provider-proxy models-store provider)
      (let ((entry (and (pair? models-store) (assoc provider models-store))))
        (and entry (let ((pair (assoc "httpProxy" (cdr entry)))) (and pair (cdr pair))))
      ) ;let
    ) ;define

    ;; 生效代理：--proxy > provider 文件 httpProxy > HTTPS_PROXY/ALL_PROXY > settings.json
    (define (pi-resolve-effective-proxy cli-proxy models-store provider settings . opt-env)
      (let ((cli
              (and (string? cli-proxy)
                (not (string-null? (string-trim-both cli-proxy)))
                cli-proxy
              ) ;and
            ) ;cli
           ) ;
        (or cli
          (pi-provider-proxy models-store provider)
          (if (pair? opt-env)
            (pi-resolve-proxy-url #f settings (car opt-env))
            (pi-resolve-proxy-url #f settings)
          ) ;if
        ) ;or
      ) ;let
    ) ;define

    ;; 全局代理链：--proxy > HTTPS_PROXY > ALL_PROXY > settings.json httpProxy
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
    ;; 元数据缺失时默认支持（对齐上游：无模型信息时返回全集级别）。
    ;; JSON 布尔为符号 true/false，必须归一化后再判真假，
    ;; 否则 reasoning: false 会被当成真值。
    (define (%model-supports-thinking? models-store provider model-id)
      (let ((model
              (find (lambda (entry) (equal? (json-dict-ref entry "id" #f) model-id))
                (pi-catalog-provider-models models-store provider)
              ) ;find
            ) ;model
           ) ;
        (if model (pi-model-supports-reasoning? model) #t)
      ) ;let
    ) ;define

    ;; 公开包装：/model 切换后重算 supports-thinking（worker 与 TUI 共用）
    (define (pi-model-supports-thinking? models-store provider model-id)
      (%model-supports-thinking? models-store provider model-id)
    ) ;define

    ;; 模型可用思考级别集（对齐上游 getSupportedThinkingLevels）：
    ;; 目录条目缺失时返回全集（对齐上游：无模型信息时返回全集级别）
    (define (pi-model-available-thinking-levels models-store provider model-id)
      (let ((model
              (find (lambda (entry) (equal? (json-dict-ref entry "id" #f) model-id))
                (pi-catalog-provider-models models-store provider)
              ) ;find
            ) ;model
           ) ;
        (if model (pi-model-thinking-levels model) pi-thinking-levels)
      ) ;let
    ) ;define

    ;; 按指定 provider 取协议标识（会话内 /model 切换后调用）。
    ;; 缺失时回退注册表内置值，保证分发不沿用旧 provider 的实现。
    (define (pi-cfg-provider-api cfg provider)
      (let* ((catalog (cdr (or (assoc 'models-store cfg) (cons #f '()))))
             (entry (and (pair? catalog) (assoc provider catalog)))
             (declared
               (and entry (let ((v (assoc "api" entry))) (and v (cdr v))))
             ) ;declared
            ) ;
        (or declared (pi-provider-api provider))
      ) ;let*
    ) ;define

    ;; 按指定 provider 重算生效代理（会话内 /model 切换后调用）
    (define (pi-cfg-provider-proxy cfg provider)
      (let ((catalog (cdr (or (assoc 'models-store cfg) (cons #f '()))))
            (settings (cdr (or (assoc 'settings cfg) (cons #f '()))))
            (cli (cdr (or (assoc 'cli-proxy cfg) (cons #f '()))))
           ) ;
        (pi-resolve-effective-proxy cli catalog provider settings)
      ) ;let
    ) ;define

    (define (resolve-pi-config parsed-args . opt-env)
      (let* ((env-agent-dir (lookup-env-var "PI_CODING_AGENT_DIR" opt-env))
             (agent-dir (pi-get-agent-dir env-agent-dir))
             ;; JSON 配置文件目录：~/.gf/agent 存在时优先，否则回退 ~/.pi/agent
             (config-dir (gf-get-config-dir env-agent-dir))
             (settings (pi-load-settings config-dir))
             ;; 模型目录来自用户配置：models/providers.json 白名单 + models/ 下的
             ;; provider 与模型文件，白名单外的 provider/模型不可用
             (models-store (pi-build-user-catalog (path->string config-dir)))
             (cli-proxy (pi-args-ref parsed-args 'proxy))
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
          ;; provider 级代理：--proxy > models/<provider>.json httpProxy
          ;; > HTTPS_PROXY/ALL_PROXY > settings.json httpProxy
          (let* ((proxy-url (if (pair? opt-env)
                              (pi-resolve-effective-proxy cli-proxy
                                models-store
                                provider
                                settings
                                (car opt-env)
                              ) ;pi-resolve-effective-proxy
                              (pi-resolve-effective-proxy cli-proxy models-store provider settings)
                            ) ;if
                 ) ;proxy-url
                 (auth (if (pair? opt-env)
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
                 ;; per-model 覆盖：settings.modelThinkingLevels["provider/modelId"]
                 (per-model-thinking (and settings (pi-settings-model-thinking-level settings provider model-id))
                 ) ;per-model-thinking
                 (settings-thinking (and settings (pi-settings-default-thinking-level settings)))
                 ;; 生效级别：CLI > per-model > 全局默认 > 默认级别（medium），
                 ;; 最后按模型可用级别收敛（对齐上游 resolve 顺序与 clamp）
                 (available-levels (pi-model-available-thinking-levels models-store provider model-id)
                 ) ;available-levels
                 (thinking-level (pi-resolve-thinking-level cli-thinking
                                   per-model-thinking settings-thinking
                                   available-levels
                                 ) ;pi-resolve-thinking-level
                 ) ;thinking-level
                 (supports-thinking (%model-supports-thinking? models-store provider model-id))
                ) ;
            (list (cons 'agent-dir agent-dir)
              (cons 'config-dir config-dir)
              (cons 'provider provider)
              (cons 'provider-api
                (let ((p-entry (pi-user-provider-config models-store provider)))
                  (and p-entry (let ((v (assoc "api" p-entry))) (and v (cdr v))))
                ) ;let
              ) ;cons
              (cons 'api-key api-key)
              (cons 'model-id model-id)
              (cons 'model-meta meta)
              (cons 'models-store models-store)
              (cons 'thinking-level thinking-level)
              (cons 'supports-thinking supports-thinking)
              (cons 'cli-proxy cli-proxy)
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
