;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api openai-completions msg)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii list)
    (liii pi ai message)
    (liii pi ai event)
    (liii pi ai tools types)
    (liii pi ai tools record)
  ) ;import
  (export openai-build-tool-declarations openai-build-messages
    openai-build-request-body openai-clamp-prompt-cache-key
    openai-resolve-prompt-cache-key
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

    (define (openai-build-tool-declarations tool-defs)
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
            (list (cons "type" "function")
              (cons "function"
                (list (cons "name" name) (cons "description" desc) (cons "parameters" params))
              ) ;cons
            ) ;list
          ) ;let
        ) ;lambda
        tool-defs
      ) ;map
    ) ;define

    (define (%block->text blk)
      (cond ((string? blk) blk)
            ((pi-text-block? blk) (pi-text-block-text blk))
            ((pair? blk) (%val 'text blk ""))
            (else "")
      ) ;cond
    ) ;define

    (define (%msg-text-content content)
      (cond ((string? content) content)
            ((vector? content) (%msg-text-content (vector->list content)))
            ((pair? content) (apply string-append (map %block->text content)))
            (else "")
      ) ;cond
    ) ;define

    (define (%assistant-text-block? blk)
      (or (pi-text-block? blk) (equal? (%val 'type blk "") "text"))
    ) ;define

    (define (%normalize-json-keys-to-strings data)
      (cond
       ((pair? data)
        (if (pair? (car data))
          (map
            (lambda (p)
              (let* ((key (car p))
                     (str-key (if (symbol? key) (symbol->string key) key))
                     (val (cdr p))
                    ) ;
                (cons str-key (%normalize-json-keys-to-strings val))
              ) ;let*
            ) ;lambda
            data
          ) ;map
          (let* ((key (car data)) (str-key (if (symbol? key) (symbol->string key) key)))
            (cons str-key (%normalize-json-keys-to-strings (cdr data)))
          ) ;let*
        ) ;if
       ) ;
       ((vector? data)
        (list->vector (map %normalize-json-keys-to-strings (vector->list data)))
       ) ;
       (else data)
      ) ;cond
    ) ;define

    (define (%ensure-strict-json-string obj)
      (cond
       ((string? obj)
        (let ((parsed (catch #t (lambda () (string->json obj)) (lambda _ #f))))
          (if (and parsed (not (eof-object? parsed)))
            (json->string (%normalize-json-keys-to-strings parsed))
            obj
          ) ;if
        ) ;let
       ) ;
       ((pair? obj) (json->string (%normalize-json-keys-to-strings obj)))
       (else (json->string obj))
      ) ;cond
    ) ;define

    (define (%assistant-block->tool-call blk)
      (and (or (pi-tool-call-block? blk) (equal? (%val 'type blk "") "toolCall"))
        (let* ((id (if (pi-tool-call-block? blk) (pi-tool-call-block-id blk) (%val 'id blk ""))
               ) ;id
               (name (if (pi-tool-call-block? blk) (pi-tool-call-block-name blk) (%val 'name blk ""))
               ) ;name
               (raw-args (if (pi-tool-call-block? blk)
                           (pi-tool-call-block-arguments blk)
                           (%val 'arguments blk '())
                         ) ;if
               ) ;raw-args
               (args-str (%ensure-strict-json-string raw-args))
              ) ;
          (list (cons "id" id)
            (cons "type" "function")
            (cons "function" (list (cons "name" name) (cons "arguments" args-str)))
          ) ;list
        ) ;let*
      ) ;and
    ) ;define

    (define (%convert-assistant-msg msg)
      (let* ((raw-c (%val 'content msg '()))
             (c-list (cond ((vector? raw-c) (vector->list raw-c))
                           ((pair? raw-c) raw-c)
                           (else '())
                     ) ;cond
             ) ;c-list
             (text-acc (%msg-text-content (filter %assistant-text-block? c-list)))
             (tc-acc (filter (lambda (tc) tc) (map %assistant-block->tool-call c-list)))
            ) ;
        ;; ESC abort 后可能残留空 content 的 assistant 消息（stopReason=aborted），
        ;; 空 assistant 消息会被部分 API 拒绝：返回 #f 跳过
        ;; （对齐上游 blocks 为空时 continue，允许产生连续 user 消息）
        (cond ((and (null? tc-acc) (string-null? text-acc)) #f)
              ((pair? tc-acc)
               (list (cons "role" "assistant")
                 (cons "content" text-acc)
                 (cons "tool_calls" (list->vector tc-acc))
               ) ;list
              ) ;
              (else (list (cons "role" "assistant") (cons "content" text-acc)))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%convert-message msg)
      (let ((role (%val 'role msg "")))
        (cond
         ((equal? role "user")
          (list (cons "role" "user")
            (cons "content" (%msg-text-content (%val 'content msg "")))
          ) ;list
         ) ;
         ((equal? role "assistant") (%convert-assistant-msg msg))
         ((equal? role "toolResult")
          (let ((call-id (if (pi-tool-result-message? msg)
                           (pi-tool-result-message-tool-call-id msg)
                           (%val 'toolCallId msg "")
                         ) ;if
                ) ;call-id
                (text (if (pi-tool-result-message? msg)
                        (%msg-text-content (pi-tool-result-message-content msg))
                        (%msg-text-content (%val 'content msg ""))
                      ) ;if
                ) ;text
               ) ;
            (list (cons "role" "tool") (cons "tool_call_id" call-id) (cons "content" text))
          ) ;let
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (openai-build-messages messages system-instruction)
      (let ((converted (filter (lambda (msg) msg) (map %convert-message messages))))
        (list->vector
          (if
            (and (string? system-instruction)
              (not (string-null? (string-trim-both system-instruction)))
            ) ;and
            (cons (list (cons "role" "system") (cons "content" system-instruction))
              converted
            ) ;cons
            converted
          ) ;if
        ) ;list->vector
      ) ;let
    ) ;define

    ;; OpenAI prompt_cache_key 最大长度（对齐上游 OPENAI_PROMPT_CACHE_KEY_MAX_LENGTH）
    (define %prompt-cache-key-max-length 64)

    (define (openai-clamp-prompt-cache-key key)
      (if (and (string? key) (> (string-length key) %prompt-cache-key-max-length))
        (substring key 0 %prompt-cache-key-max-length)
        key
      ) ;if
    ) ;define

    ;; 仅 OpenAI 官方端点注入：第三方兼容端点对未知字段可能报错
    (define (openai-resolve-prompt-cache-key base-url session-id)
      (if (and (string? base-url)
            (string-contains? base-url "api.openai.com")
            (string? session-id)
            (> (string-length session-id) 0)
          ) ;and
        (openai-clamp-prompt-cache-key session-id)
        #f
      ) ;if
    ) ;define

    (define* (openai-build-request-body model-id
               messages
               stream?
               tool-decls
               max-tokens
               (prompt-cache-key #f)
             ) ;openai-build-request-body
      (let* ((body (list (cons "model" model-id) (cons "messages" messages)))
             (body
               (if stream? (append body (list (cons "stream" #t))) body)
             ) ;body
             (body
               (if (and (integer? max-tokens) (> max-tokens 0))
                 (append body (list (cons "max_tokens" max-tokens)))
                 body
               ) ;if
             ) ;body
             (body
               (if (and (pair? tool-decls) (> (length tool-decls) 0))
                 (append body (list (cons "tools" (list->vector tool-decls))))
                 body
               ) ;if
             ) ;body
             (body
               (if (and (string? prompt-cache-key) (> (string-length prompt-cache-key) 0))
                 (append body (list (cons "prompt_cache_key" prompt-cache-key)))
                 body
               ) ;if
             ) ;body
            ) ;
        body
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
