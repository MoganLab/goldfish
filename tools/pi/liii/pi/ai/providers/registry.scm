;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers registry)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii pi ai providers anthropic)
    (liii pi ai providers deepseek)
    (liii pi ai providers glm)
    (liii pi ai providers google)
    (liii pi ai providers kimi-coding)
    (liii pi ai providers minimax-cn)
    (liii pi ai providers zai-coding-cn)
    (liii pi ai providers quota)
  ) ;import
  (export PI_PROVIDERS PI_SUPPORTED_PROVIDERS PI_PROVIDER_BUILTIN_MODELS
    pi-provider-record pi-provider-api pi-provider-base-url pi-provider-env-keys
    pi-provider-builtin-models pi-provider-quota-error-handler
    pi-provider-quota-error
  ) ;export
  (begin

    ;; 全部 provider 注册表：id -> 配置（api/baseUrl/envKeys）。
    ;; "zai" 为 "zai-coding-cn" 的路由别名，共享同一配置。
    (define PI_PROVIDERS
      (list (cons "anthropic" PI_ANTHROPIC_PROVIDER)
        (cons "deepseek" PI_DEEPSEEK_PROVIDER)
        (cons "glm" PI_GLM_PROVIDER)
        (cons "google" PI_GOOGLE_PROVIDER)
        (cons "kimi-coding" PI_KIMI_CODING_PROVIDER)
        (cons "minimax-cn" PI_MINIMAX_CN_PROVIDER)
        (cons "zai-coding-cn" PI_ZAI_CODING_CN_PROVIDER)
        (cons "zai" PI_ZAI_CODING_CN_PROVIDER)
      ) ;list
    ) ;define

    (define PI_SUPPORTED_PROVIDERS
      '("google" "anthropic" "kimi-coding" "glm" "deepseek" "minimax-cn"
        "zai-coding-cn")
    ) ;define

    ;; 各 provider 的内置模型目录（id -> models），与 PI_PROVIDERS 同源
    (define PI_PROVIDER_BUILTIN_MODELS
      (list (cons "anthropic" PI_ANTHROPIC_BUILTIN_MODELS)
        (cons "deepseek" PI_DEEPSEEK_BUILTIN_MODELS)
        (cons "google" PI_GOOGLE_BUILTIN_MODELS)
        (cons "kimi-coding" PI_KIMI_CODING_BUILTIN_MODELS)
        (cons "minimax-cn" PI_MINIMAX_CN_BUILTIN_MODELS)
        (cons "zai-coding-cn" PI_ZAI_CODING_CN_BUILTIN_MODELS)
      ) ;list
    ) ;define

    (define (%p-str provider)
      (if (symbol? provider) (symbol->string provider) provider)
    ) ;define

    (define (%field record key default-val)
      (let ((pair (assoc key record)))
        (if pair (cdr pair) default-val)
      ) ;let
    ) ;define

    ;; 按 id 查 provider 配置，未注册返回 #f
    (define (pi-provider-record provider)
      (let ((pair (assoc (%p-str provider) PI_PROVIDERS)))
        (and pair (cdr pair))
      ) ;let
    ) ;define

    ;; provider 使用的协议实现标识：anthropic-messages / google-generative-ai /
    ;; openai-completions；未注册 provider 默认走 google-generative-ai
    (define (pi-provider-api provider)
      (let ((record (pi-provider-record provider)))
        (if record (%field record 'api "google-generative-ai") "google-generative-ai")
      ) ;let
    ) ;define

    (define (pi-provider-base-url provider)
      (let ((record (pi-provider-record provider)))
        (and record (%field record 'baseUrl #f))
      ) ;let
    ) ;define

    (define (pi-provider-env-keys provider)
      (let ((record (pi-provider-record provider)))
        (if record (%field record 'envKeys '()) '())
      ) ;let
    ) ;define

    ;; 各 provider 注册的「额度耗尽」识别函数（解析错误码与响应 JSON）
    ;; 函数签名：(handler status-code body-str) -> 结果 alist 或 #f
    (define PI_PROVIDER_QUOTA_ERROR_HANDLERS
      (list (cons "zai-coding-cn" PI_ZAI_CODING_CN_QUOTA_ERROR)
        (cons "zai" PI_ZAI_CODING_CN_QUOTA_ERROR)
        (cons "kimi-coding" PI_KIMI_CODING_QUOTA_ERROR)
        (cons "minimax-cn" PI_MINIMAX_CN_QUOTA_ERROR)
      ) ;list
    ) ;define

    ;; 取某个 provider 注册的额度耗尽识别函数；未注册返回 #f
    (define (pi-provider-quota-error-handler provider)
      (let* ((key
               (cond ((string? provider) (string-downcase (string-trim-both provider)))
                     ((symbol? provider) (string-downcase (symbol->string provider)))
                     (else #f)
               ) ;cond
             ) ;key
             (pair (and key (assoc key PI_PROVIDER_QUOTA_ERROR_HANDLERS)))
            ) ;
        (and pair (cdr pair))
      ) ;let*
    ) ;define

    ;; 调用某个 provider 的额度耗尽识别函数；未注册或出错时返回 #f
    (define* (pi-provider-quota-error status-code body-str (provider #f))
      (let ((handler (pi-provider-quota-error-handler provider)))
        (if (procedure? handler)
          (catch #t (lambda () (handler status-code body-str)) (lambda args #f))
          #f
        ) ;if
      ) ;let
    ) ;define*

    (define (pi-provider-builtin-models provider)
      (let ((pair (assoc (%p-str provider) PI_PROVIDER_BUILTIN_MODELS)))
        (if pair (cdr pair) '())
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
