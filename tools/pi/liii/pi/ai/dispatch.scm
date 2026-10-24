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
  ) ;import
  (export pi-request-provider-stream)
  (begin

    ;; 按 provider 注册表声明的协议（api 字段）把请求路由到对应协议实现
    ;; （对应上游 pi-ai 中 provider.api 的路由职责；agent 层经 request-fn
    ;; 参数引用本函数，自身不依赖具体 provider）
    (define (pi-request-provider-stream provider config msgs sys-prompt tools on-ev timeout)
      (let ((api (pi-provider-api provider)))
        (cond ((string=? api "anthropic-messages")
               (anthropic-request-stream config msgs sys-prompt tools on-ev timeout)
              ) ;
              ((string=? api "openai-completions")
               (openai-request-stream config msgs sys-prompt tools on-ev timeout)
              ) ;
              (else (gemini-request-stream config msgs sys-prompt tools on-ev timeout))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
