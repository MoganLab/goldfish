;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai dispatch)
  (import (scheme base)
    (liii pi ai gemini)
    (liii pi ai anthropic)
    (liii pi ai kimi)
    (liii pi ai deepseek)
    (liii pi ai minimax)
    (liii pi ai zai)
  ) ;import
  (export pi-request-provider-stream)
  (begin

    ;; 按 provider 名把请求路由到对应的流式实现（对应上游 pi-ai Models 集合的
    ;; 路由职责；agent 层经 request-fn 参数引用本函数，自身不依赖具体 provider）
    (define (pi-request-provider-stream provider config msgs sys-prompt tools on-ev timeout)
      (cond ((or (equal? provider "anthropic") (equal? provider "glm"))
             (anthropic-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            ((equal? provider "deepseek")
             (deepseek-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            ((equal? provider "kimi-coding")
             (kimi-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            ((or (equal? provider "zai-coding-cn") (equal? provider "zai"))
             (zai-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            ((or (equal? provider "minimax") (equal? provider "minimax-cn"))
             (minimax-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            (else (gemini-request-stream config msgs sys-prompt tools on-ev timeout))
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
