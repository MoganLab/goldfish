;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages core)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii json)
    (liii error)
    (liii pi ai message)
    (liii pi ai event)
    (liii pi ai tools types)
    (liii pi ai tools record)
    (liii pi ai limits)
    (liii pi ai api anthropic-messages input)
  ) ;import
  (export anthropic-map-stop-reason anthropic-map-usage
    anthropic-build-tool-declarations anthropic-build-messages
    anthropic-build-request-body anthropic-parse-response
    anthropic-is-retriable-error? anthropic-normalize-input
    anthropic-parse-tool-arguments
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

    (define (anthropic-map-stop-reason reason)
      (let ((r (if (symbol? reason) (symbol->string reason) reason)))
        (cond ((member r '("end_turn" "pause_turn" "stop_sequence")) "stop")
              ((equal? r "max_tokens") "length")
              ((equal? r "tool_use") "toolUse")
              (else "error")
        ) ;cond
      ) ;let
    ) ;define

    (define (anthropic-map-usage usage-meta)
      (let* ((in-tok (%val "input_tokens" usage-meta 0))
             (out-tok (%val "output_tokens" usage-meta 0))
             (c-read (%val "cache_read_input_tokens" usage-meta 0))
             (c-write (%val "cache_creation_input_tokens" usage-meta 0))
            ) ;
        (list (cons 'input in-tok)
          (cons 'output out-tok)
          (cons 'cacheRead c-read)
          (cons 'cacheWrite c-write)
          (cons 'totalTokens (+ in-tok out-tok))
        ) ;list
      ) ;let*
    ) ;define

    (define (anthropic-build-tool-declarations tool-defs)
      (map
        (lambda (def)
          (let ((params
                  (if (pi-tool-definition? def)
                    (pi-tool-definition-parameters def)
                    (or (%val 'parameters def #f)
                      (%val 'parametersJsonSchema def #f)
                      '(("type" . "object"))
                    ) ;or
                  ) ;if
                ) ;params
                (name (if (pi-tool-definition? def) (pi-tool-definition-name def) (%val 'name def ""))
                ) ;name
                (desc (if (pi-tool-definition? def)
                        (pi-tool-definition-description def)
                        (%val 'description def "")
                      ) ;if
                ) ;desc
               ) ;
            (list (cons "name" name) (cons "description" desc) (cons "input_schema" params))
          ) ;let
        ) ;lambda
        tool-defs
      ) ;map
    ) ;define

    (define %assistant-blk->anthropic-blk anthropic-assistant-blk->anthropic-blk)

    (define (%first-blk-text blk)
      (if (string? blk) blk (%val 'text blk ""))
    ) ;define

    (define (%tool-result-text content)
      (cond ((string? content) content)
            ((and (vector? content) (> (vector-length content) 0))
             (%first-blk-text (vector-ref content 0))
            ) ;
            ((pair? content) (%first-blk-text (car content)))
            (else "")
      ) ;cond
    ) ;define

    (define (%tool-result->anthropic-blk msg)
      (let ((text (%tool-result-text (%val 'content msg "")))
            (is-err? (%val 'isError msg #f))
           ) ;
        (list (cons "type" "tool_result")
          (cons "tool_use_id" (%val 'toolCallId msg ""))
          (cons "content" text)
          (cons "is_error" (if is-err? #t #f))
        ) ;list
      ) ;let
    ) ;define

    (define (%take-tool-results rest acc)
      (if
        (and (pair? rest) (equal? (%val 'role (car rest) "") "toolResult"))
        (%take-tool-results (cdr rest)
          (cons (%tool-result->anthropic-blk (car rest)) acc)
        ) ;%take-tool-results
        (values rest acc)
      ) ;if
    ) ;define

    (define (%build-turn-msg role content-vec)
      (list (cons "role" role) (cons "content" content-vec))
    ) ;define

    (define (anthropic-build-messages messages)
      (let loop
        ((rest messages) (acc '()))
        (if (null? rest)
          (list->vector (reverse acc))
          (let* ((msg (car rest)) (role (%val 'role msg "")))
            (cond
             ((equal? role "toolResult")
              (let*-values (((rem rblocks) (%take-tool-results rest '()))
                            ((b-vec) (list->vector (reverse rblocks)))
                           ) ;
                (loop rem (cons (%build-turn-msg "user" b-vec) acc))
              ) ;let*-values
             ) ;
             ((equal? role "user")
              (let* ((c (%val 'content msg ""))
                     (cvec
                       (if (vector? c) c (vector (list (cons "type" "text") (cons "text" c))))
                     ) ;cvec
                    ) ;
                (loop (cdr rest) (cons (%build-turn-msg "user" cvec) acc))
              ) ;let*
             ) ;
             ((equal? role "assistant")
              (let* ((c (%val 'content msg '()))
                     (c-list (cond ((vector? c) (vector->list c)) ((pair? c) c) (else '())))
                     (blks (map %assistant-blk->anthropic-blk c-list))
                    ) ;
                ;; ESC abort 后可能残留空 content 的 assistant 消息（stopReason=aborted），
                ;; 空 assistant 消息会被 API 拒绝（HTTP 400 must not be empty）：
                ;; 整体跳过（对齐上游 blocks 为空时 continue），允许产生连续 user 消息
                (if (null? blks)
                  (loop (cdr rest) acc)
                  (loop (cdr rest) (cons (%build-turn-msg "assistant" (list->vector blks)) acc))
                ) ;if
              ) ;let*
             ) ;
             (else (loop (cdr rest) acc))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (anthropic-build-request-body model-id messages system-prompt
              tool-decls max-tokens
            ) ;anthropic-build-request-body
      (let* ((b (list (cons "model" model-id)
                  (cons "max_tokens" (or max-tokens 8192))
                  (cons "messages" messages)
                ) ;list
             ) ;b
             (b
               (if
                 (and (string? system-prompt)
                   (not (string-null? (string-trim-both system-prompt)))
                 ) ;and
                 (append b (list (cons "system" system-prompt)))
                 b
               ) ;if
             ) ;b
            ) ;
        (if (and (pair? tool-decls) (> (length tool-decls) 0))
          (append b (list (cons "tools" (list->vector tool-decls))))
          b
        ) ;if
      ) ;let*
    ) ;define

    (define (%extract-blocks raw-content)
      (let* ((content-vec (cond ((vector? raw-content) raw-content)
                                ((list? raw-content) (list->vector raw-content))
                                (else #())
                          ) ;cond
             ) ;content-vec
            ) ;
        (filter (lambda (b) b)
          (map
            (lambda (blk)
              (let ((btype (%val "type" blk "")))
                (cond ((equal? btype "text") (pi-text-block (%val "text" blk "")))
                      ((equal? btype "tool_use")
                       (pi-tool-call-block (%val "id" blk "")
                         (%val "name" blk "")
                         (anthropic-parse-tool-arguments (%val "input" blk '()))
                       ) ;pi-tool-call-block
                      ) ;
                      ((equal? btype "thinking")
                       (pi-thinking-block (%val "thinking" blk "") (%val "signature" blk #f))
                      ) ;
                      (else #f)
                ) ;cond
              ) ;let
            ) ;lambda
            (vector->list content-vec)
          ) ;map
        ) ;filter
      ) ;let*
    ) ;define

    (define (anthropic-parse-response json-data status-code)
      (if (not (json-object? json-data))
        (pi-assistant-message (list (pi-text-block ""))
          :stop-reason   "error"
          :usage         (anthropic-map-usage '())
          :error-message "Invalid JSON response from Anthropic API"
        ) ;pi-assistant-message
        (let* ((err (%val "error" json-data #f))
               (raw-stop (%val "stop_reason" json-data "end_turn"))
               (usage (anthropic-map-usage (%val "usage" json-data '())))
              ) ;
          (if (pair? err)
            (pi-assistant-message (list (pi-text-block ""))
              :stop-reason   "error"
              :usage         usage
              :error-message (%val "message" err "API error")
            ) ;pi-assistant-message
            (let* ((raw-c (%val "content" json-data #()))
                   (blocks (%extract-blocks raw-c))
                   (stop-reason (anthropic-map-stop-reason raw-stop))
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

    ;; 额度耗尽（5 小时窗口 / 周额度等）不是瞬时错误，重试无意义
    (define* (anthropic-is-retriable-error? status-code response-body-str (provider #f))
      (if (pi-usage-limit-error? status-code response-body-str provider)
        #f
        (or (= status-code 408)
          (= status-code 409)
          (= status-code 429)
          (= status-code 529)
          (>= status-code 500)
          (zero? status-code)
        ) ;or
      ) ;if
    ) ;define*

  ) ;begin
) ;define-library
