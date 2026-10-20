;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi zai)
  (import (scheme base) (liii string) (liii pi kimi))
  (export zai-map-stop-reason zai-map-usage zai-build-tool-declarations
    zai-build-messages zai-build-request-body zai-parse-response
    zai-is-retriable-error? zai-build-tool-declarations-from-names zai-request
    zai-request-stream zai-build-endpoint zai-build-headers
    zai-build-proxy-alist
  ) ;export
  (begin

    ;; ZAI / 智谱 BigModel 的 Coding Copilot 端点与 OpenAI Chat Completions 协议一致，
    ;; 因此复用 (liii pi kimi) 的实现，本库提供 provider 命名的薄封装。
    (define zai-map-stop-reason kimi-map-stop-reason)
    (define zai-map-usage kimi-map-usage)
    (define zai-build-tool-declarations kimi-build-tool-declarations)
    (define zai-build-messages kimi-build-messages)
    (define zai-build-request-body kimi-build-request-body)
    (define (zai-parse-response json-data . opt-status)
      (let ((status (if (pair? opt-status) (car opt-status) 200)))
        (kimi-parse-response json-data status)
      ) ;let
    ) ;define
    (define (zai-is-retriable-error? status-code . opt-body)
      (let ((body (if (pair? opt-body) (car opt-body) "")))
        (kimi-is-retriable-error? status-code body)
      ) ;let
    ) ;define
    (define zai-build-tool-declarations-from-names
      kimi-build-tool-declarations-from-names
    ) ;define
    (define zai-request kimi-request)
    (define zai-request-stream kimi-request-stream)
    (define (zai-build-endpoint base-url)
      (let* ((raw (or base-url "https://open.bigmodel.cn/api/coding/paas/v4"))
             (b
               (if (string-ends? raw "/") (substring raw 0 (- (string-length raw) 1)) raw)
             ) ;b
            ) ;
        (cond ((string-ends? b "/chat/completions") b)
              ((or (string-ends? b "/v1") (string-ends? b "/v4"))
               (string-append b "/chat/completions")
              ) ;
              (else (string-append b "/chat/completions"))
        ) ;cond
      ) ;let*
    ) ;define
    (define zai-build-headers kimi-build-headers)
    (define zai-build-proxy-alist kimi-build-proxy-alist)

  ) ;begin
) ;define-library
