;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai minimax)
  (import (scheme base) (liii pi ai anthropic))
  (export minimax-map-stop-reason minimax-map-usage
    minimax-build-tool-declarations minimax-build-messages
    minimax-build-request-body minimax-parse-response
    minimax-is-retriable-error? minimax-build-tool-declarations-from-names
    minimax-request minimax-request-stream
  ) ;export
  (begin

    ;; MiniMax 的 Anthropic 兼容端点与 Claude 协议一致（消息、工具调用、
    ;; thinking、SSE 事件形状均相同），因此复用 (liii pi ai anthropic) 的实现，
    ;; 本库只提供 provider 命名的薄封装（与 (liii pi ai deepseek) 同型）。
    (define minimax-map-stop-reason anthropic-map-stop-reason)
    (define minimax-map-usage anthropic-map-usage)
    (define minimax-build-tool-declarations anthropic-build-tool-declarations)
    (define minimax-build-messages anthropic-build-messages)
    (define minimax-build-request-body anthropic-build-request-body)
    (define (minimax-parse-response json-data . opt-status)
      (let ((status (if (pair? opt-status) (car opt-status) 200)))
        (anthropic-parse-response json-data status)
      ) ;let
    ) ;define
    (define (minimax-is-retriable-error? status-code . opt-body)
      (let ((body (if (pair? opt-body) (car opt-body) "")))
        (anthropic-is-retriable-error? status-code body)
      ) ;let
    ) ;define
    (define minimax-build-tool-declarations-from-names
      anthropic-build-tool-declarations-from-names
    ) ;define
    (define minimax-request anthropic-request)
    (define minimax-request-stream anthropic-request-stream)

  ) ;begin
) ;define-library
