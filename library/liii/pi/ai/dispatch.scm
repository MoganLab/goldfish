;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai dispatch)
  (import (scheme base)
    (liii pi ai api anthropic-messages)
    (liii pi ai api google-generative-ai)
    (liii pi ai api openai-completions)
    (liii pi ai config types)
    (liii pi ai config record)
    (liii pi ai providers registry)
    (liii pi ai transcript)
  ) ;import
  (export pi-request-provider-stream pi-request-provider pi-make-request-config)
  (begin

    ;; 构造标准请求配置 alist：根据 provider / model-id / api-key 自动推导默认
    ;; 协议 (provider-api) 与端点 (base-url)，同时填充 model-meta、tools、
    ;; proxy-url、settings 等下游必需字段，支持关键字参数覆盖。
    (define* (pi-make-request-config provider
               model-id
               api-key
               (base-url #f)
               (provider-api #f)
               (max-tokens 4096)
               (tools '())
               (proxy-url #f)
               (settings '())
             ) ;pi-make-request-config
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (eff-api (or provider-api (pi-provider-api p-str)))
             (eff-base-url (or base-url (pi-provider-base-url p-str) ""))
             ;; model-meta 统一为 record（issue #1172 后续收敛）
             (meta (pi-model-metadata (or model-id "")
                     (or model-id "")
                     p-str
                     eff-base-url
                     #f
                     (or max-tokens 4096)
                     #t
                   ) ;pi-model-metadata
             ) ;meta
            ) ;
        (list (cons 'provider p-str)
          (cons 'provider-api eff-api)
          (cons 'model-id model-id)
          (cons 'api-key api-key)
          (cons 'model-meta meta)
          (cons 'tools (or tools '()))
          (cons 'proxy-url proxy-url)
          (cons 'settings (or settings '()))
        ) ;list
      ) ;let*
    ) ;define*

    ;; 规范化配置：补齐 downstream 必需字段，防止缺失键触发 type-error
    (define (%normalize-config config provider api tools)
      (let* ((cfg (if (pair? config) config '()))
             (p-pair (or (assoc 'provider cfg) (assoc "provider" cfg)))
             (c1 (if p-pair cfg (cons (cons 'provider provider) cfg)))
             (api-pair (or (assoc 'provider-api c1) (assoc "provider-api" c1)))
             (c2 (if api-pair c1 (cons (cons 'provider-api api) c1)))
             (tools-pair (or (assoc 'tools c2) (assoc "tools" c2)))
             (c3
               (if tools-pair c2 (cons (cons 'tools (or tools '())) c2))
             ) ;c3
             (proxy-pair (or (assoc 'proxy-url c3) (assoc "proxy-url" c3)))
             (c4 (if proxy-pair c3 (cons (cons 'proxy-url #f) c3)))
             (settings-pair (or (assoc 'settings c4) (assoc "settings" c4)))
             (c5 (if settings-pair c4 (cons (cons 'settings '()) c4)))
             (meta-pair (or (assoc 'model-meta c5) (assoc "model-meta" c5)))
             (raw-meta (and meta-pair (cdr meta-pair)))
             ;; model-meta 统一为 record：旧 alist（含会话 JSON 载入）规范化，
             ;; 缺失时用注册表默认值补齐；不再把 record 降级为 alist
             (c6
               (let ((m
                      (cond (raw-meta
                             (pi-model-metadata-from-alist raw-meta)
                            ) ;
                            (else
                              (pi-model-metadata ""
                                ""
                                provider
                                (or (pi-provider-base-url provider) "")
                                #f
                                4096
                                #t
                              ) ;pi-model-metadata
                            ) ;else
                      ) ;cond
                    ) ;m
                   ) ;
                 (cons (cons 'model-meta m) c5)
               ) ;let
             ) ;c6
            ) ;
        c6
      ) ;let*
    ) ;define

    ;; 按 provider 注册表声明的协议（api 字段）把请求路由到对应协议实现
    ;; （对应上游 pi-ai 中 provider.api 的路由职责；agent 层经 request-fn
    ;; 参数引用本函数，自身不依赖具体 provider）
    ;; 协议选择：优先用 config 中已解析的 provider-api（支持自定义 provider 的
    ;; models/<id>.json 声明），缺失时回退注册表内置值
    ;; 非流式请求：与流式入口共享协议路由和配置规范化，直接返回一次完整响应。
    ;; opt-retry 可显式禁用自动重试，供轻量探活保持单次、快速失败。
    (define* (pi-request-provider provider
               config
               msgs
               sys-prompt
               tools
               timeout
               (opt-retry #f)
             ) ;pi-request-provider
      (let* ((cfg-p
               (and (pair? config)
                 (let ((p (or (assoc 'provider config) (assoc "provider" config))))
                   (and p (cdr p))
                 ) ;let
               ) ;and
             ) ;cfg-p
             (eff-provider (or provider cfg-p "google"))
             (eff-p-str (if (symbol? eff-provider) (symbol->string eff-provider) eff-provider)
             ) ;eff-p-str
             (cfg-api
               (and (pair? config)
                 (let ((v (or (assoc 'provider-api config) (assoc "provider-api" config))))
                   (and v (cdr v))
                 ) ;let
               ) ;and
             ) ;cfg-api
             (api (or cfg-api (pi-provider-api eff-p-str)))
             (eff-config (%normalize-config config eff-p-str api tools))
             (norm-msgs (pi-normalize-transcript msgs eff-config))
            ) ;
        (cond ((string=? api "anthropic-messages")
               (anthropic-request eff-config norm-msgs sys-prompt tools timeout opt-retry)
              ) ;
              ((string=? api "openai-completions")
               (openai-request eff-config norm-msgs sys-prompt tools timeout opt-retry)
              ) ;
              (else (gemini-request eff-config norm-msgs sys-prompt tools timeout opt-retry))
        ) ;cond
      ) ;let*
    ) ;define*

    (define (pi-request-provider-stream provider config msgs sys-prompt tools on-ev timeout)
      (let* ((cfg-p
               (and (pair? config)
                 (let ((p (or (assoc 'provider config) (assoc "provider" config))))
                   (and p (cdr p))
                 ) ;let
               ) ;and
             ) ;cfg-p
             (eff-provider (or provider cfg-p "google"))
             (eff-p-str (if (symbol? eff-provider) (symbol->string eff-provider) eff-provider)
             ) ;eff-p-str
             (cfg-api
               (and (pair? config)
                 (let ((v (or (assoc 'provider-api config) (assoc "provider-api" config))))
                   (and v (cdr v))
                 ) ;let
               ) ;and
             ) ;cfg-api
             (api (or cfg-api (pi-provider-api eff-p-str)))
             (eff-config (%normalize-config config eff-p-str api tools))
             ;; 请求前转录规范化（对齐上游 transformMessages）：跳过 error/aborted
             ;; 消息、跨模型过滤异构思维链与签名剥离、补孤儿 toolCall 的合成结果、按需归一化 toolCallId，保证请求
             ;; 前缀跨轮稳定。仅作用于请求构建，不改会话持久化。
             (norm-msgs (pi-normalize-transcript msgs eff-config))
            ) ;
        (cond ((string=? api "anthropic-messages")
               (anthropic-request-stream eff-config norm-msgs sys-prompt tools on-ev timeout)
              ) ;
              ((string=? api "openai-completions")
               (openai-request-stream eff-config norm-msgs sys-prompt tools on-ev timeout)
              ) ;
              (else (gemini-request-stream eff-config norm-msgs sys-prompt tools on-ev timeout)
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
