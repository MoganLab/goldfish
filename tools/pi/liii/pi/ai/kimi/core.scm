;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai kimi core)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii error)
    (liii pi ai message)
    (liii pi ai event)
  ) ;import
  (export kimi-map-stop-reason kimi-map-usage kimi-parse-response
    kimi-is-retriable-error?
  ) ;export
  (begin

    (define (%val key obj default-val)
      (let ((p
              (or (assoc key obj)
                (assoc (if (symbol? key) (symbol->string key) (string->symbol key)) obj)
              ) ;or
            ) ;p
           ) ;
        (if (pair? p) (cdr p) default-val)
      ) ;let
    ) ;define

    (define (kimi-map-stop-reason reason)
      (let ((r (if (symbol? reason) (symbol->string reason) reason)))
        (cond ((equal? r "stop") "stop")
              ((equal? r "length") "length")
              ((equal? r "tool_calls") "toolUse")
              (else "error")
        ) ;cond
      ) ;let
    ) ;define

    (define (kimi-map-usage usage-meta)
      (let* ((in-tok (%val "prompt_tokens" usage-meta 0))
             (out-tok (%val "completion_tokens" usage-meta 0))
             (tot-tok (%val "total_tokens" usage-meta (+ in-tok out-tok)))
             (c-read (%val "cached_tokens" usage-meta 0))
            ) ;
        (list (cons 'input in-tok)
          (cons 'output out-tok)
          (cons 'cacheRead c-read)
          (cons 'cacheWrite 0)
          (cons 'totalTokens tot-tok)
        ) ;list
      ) ;let*
    ) ;define

    (define (%extract-message-blocks msg-obj)
      (let* ((content-str (%val "content" msg-obj ""))
             (raw-tc (%val "tool_calls" msg-obj #()))
             (tc-vec (cond ((vector? raw-tc) raw-tc)
                           ((list? raw-tc) (list->vector raw-tc))
                           (else #())
                     ) ;cond
             ) ;tc-vec
             (blocks '())
            ) ;
        (when (and (string? content-str) (not (string-null? content-str)))
          (set! blocks (cons (pi-text-block content-str) blocks))
        ) ;when
        (let loop
          ((i 0))
          (when (< i (vector-length tc-vec))
            (let* ((tc (vector-ref tc-vec i))
                   (id (%val "id" tc ""))
                   (fn (%val "function" tc '()))
                   (name (%val "name" fn ""))
                   (raw-args (%val "arguments" fn "{}"))
                   (args
                     (cond
                      ((string? raw-args)
                       (catch #t (lambda () (string->json raw-args)) (lambda args '()))
                      ) ;
                      ((pair? raw-args) raw-args)
                      (else '())
                     ) ;cond
                   ) ;args
                  ) ;
              (set! blocks (cons (pi-tool-call-block id name args) blocks))
              (loop (+ i 1))
            ) ;let*
          ) ;when
        ) ;let
        (reverse blocks)
      ) ;let*
    ) ;define

    (define (kimi-parse-response json-data status-code)
      (if (not (json-object? json-data))
        (pi-assistant-message (list (pi-text-block ""))
          :stop-reason   "error"
          :usage         (kimi-map-usage '())
          :error-message "Invalid JSON response from Kimi API"
        ) ;pi-assistant-message
        (let* ((err (%val "error" json-data #f))
               (usage (kimi-map-usage (%val "usage" json-data '())))
              ) ;
          (if (pair? err)
            (pi-assistant-message (list (pi-text-block ""))
              :stop-reason   "error"
              :usage         usage
              :error-message (%val "message" err "Kimi API error")
            ) ;pi-assistant-message
            (let* ((choices (%val "choices" json-data #()))
                   (choice
                     (if (and (vector? choices) (> (vector-length choices) 0))
                       (vector-ref choices 0)
                       '()
                     ) ;if
                   ) ;choice
                   (raw-stop (%val "finish_reason" choice "stop"))
                   (msg-obj (%val "message" choice '()))
                   (blocks (%extract-message-blocks msg-obj))
                   (stop-reason (kimi-map-stop-reason raw-stop))
                  ) ;
              (pi-assistant-message blocks
                :stop-reason   stop-reason
                :usage         usage
                :error-message (if (equal? stop-reason "error") raw-stop #f)
              ) ;pi-assistant-message
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (kimi-is-retriable-error? status-code response-body-str)
      (if (memv status-code '(0 408 409 429 500 502 503 504 529)) #t #f)
    ) ;define

  ) ;begin
) ;define-library
