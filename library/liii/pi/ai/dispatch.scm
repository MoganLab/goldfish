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
    (liii pi ai providers registry)
    (liii pi ai transcript)
  ) ;import
  (export pi-request-provider-stream)
  (begin

    ;; 按 provider 注册表声明的协议（api 字段）把请求路由到对应协议实现
    ;; （对应上游 pi-ai 中 provider.api 的路由职责；agent 层经 request-fn
    ;; 参数引用本函数，自身不依赖具体 provider）
    ;; 协议选择：优先用 config 中已解析的 provider-api（支持自定义 provider 的
    ;; models/<id>.json 声明），缺失时回退注册表内置值
    (define (pi-request-provider-stream provider config msgs sys-prompt tools on-ev timeout)
      (let* ((cfg-api
               (and (pair? config) (let ((v (assoc 'provider-api config))) (and v (cdr v))))
             ) ;cfg-api
             (api (or cfg-api (pi-provider-api provider)))
            ) ;
        ;; 请求前转录规范化（对齐上游 transformMessages）：跳过 error/aborted
        ;; 消息、跨模型过滤异构思维链与签名剥离、补孤儿 toolCall 的合成结果、按需归一化 toolCallId，保证请求
        ;; 前缀跨轮稳定。仅作用于请求构建，不改会话持久化。
        (let* ((cfg-with-provider (if (or (assoc 'provider config) (assoc "provider" config))
                                    config
                                    (cons (cons 'provider provider) config)
                                  ) ;if
               ) ;cfg-with-provider
               (cfg-with-api (if (or (assoc 'provider-api cfg-with-provider)
                                   (assoc "provider-api" cfg-with-provider)
                                 ) ;or
                               cfg-with-provider
                               (cons (cons 'provider-api api) cfg-with-provider)
                             ) ;if
               ) ;cfg-with-api
               (norm-msgs (pi-normalize-transcript msgs cfg-with-api))
              ) ;
          (cond ((string=? api "anthropic-messages")
                 (anthropic-request-stream config norm-msgs sys-prompt tools on-ev timeout)
                ) ;
                ((string=? api "openai-completions")
                 (openai-request-stream config norm-msgs sys-prompt tools on-ev timeout)
                ) ;
                (else (gemini-request-stream config norm-msgs sys-prompt tools on-ev timeout))
          ) ;cond
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
