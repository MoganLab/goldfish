;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii pi ai time)
    (liii pi ai message block)
    (liii pi ai message text-block)
    (liii pi ai message thinking-block)
    (liii pi ai message tool-call-block)
    (liii pi ai message image-block)
    (liii pi ai message user)
    (liii pi ai message assistant)
    (liii pi ai message tool-result)
  ) ;import
  (export pi-message? pi-message-role pi-message->json json->pi-message
    pi-block? pi-block-type pi-block->json json->pi-block pi-user-message
    pi-user-message? pi-user-message-role pi-user-message-content
    pi-user-message-timestamp pi-bash-execution->user-text pi-assistant-message
    pi-assistant-message? pi-assistant-message-role
    pi-assistant-message-content pi-assistant-message-stop-reason
    pi-assistant-message-error-message pi-assistant-message-usage
    pi-assistant-message-timestamp pi-assistant-message-provider
    pi-assistant-message-model pi-assistant-message-api pi-tool-result-message
    pi-tool-result-message? pi-tool-result-message-role
    pi-tool-result-message-tool-call-id pi-tool-result-message-tool-name
    pi-tool-result-message-content pi-tool-result-message-is-error
    pi-tool-result-message-timestamp pi-text-block pi-text-block?
    pi-text-block-type pi-text-block-text pi-text-block-signature
    pi-thinking-block pi-thinking-block? pi-thinking-block-type
    pi-thinking-block-thinking pi-thinking-block-thinking-signature
    pi-tool-call-block pi-tool-call-block? pi-tool-call-block-type
    pi-tool-call-block-id pi-tool-call-block-name pi-tool-call-block-arguments
    pi-tool-call-block-thought-signature pi-image-block pi-image-block?
    pi-image-block-type pi-image-block-data pi-image-block-mime-type
  ) ;export
  (begin

    (define (pi-message? x)
      (or (pi-user-message? x) (pi-assistant-message? x) (pi-tool-result-message? x))
    ) ;define

    (define (pi-message-role msg)
      (cond ((pi-user-message? msg) (pi-user-message-role msg))
            ((pi-assistant-message? msg) (pi-assistant-message-role msg))
            ((pi-tool-result-message? msg) (pi-tool-result-message-role msg))
            ((pair? msg)
             (let ((p (or (assoc "role" msg) (assoc 'role msg))))
               (if p (cdr p) #f)
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-message->json msg)
      (unless (pi-message? msg)
        (type-error "pi-message->json: expected pi-message" msg)
      ) ;unless
      (cond
       ((pi-user-message? msg)
        (let ((c (%normalize-json-value (pi-user-message-content msg)))
              (ts (pi-user-message-timestamp msg))
             ) ;
          (if ts
            (list (cons "role" "user") (cons "content" c) (cons "timestamp" ts))
            (list (cons "role" "user") (cons "content" c))
          ) ;if
        ) ;let
       ) ;
       ((pi-assistant-message? msg)
        (let* ((c (%normalize-json-value (pi-assistant-message-content msg)))
               (sr (%normalize-json-value (pi-assistant-message-stop-reason msg)))
               (err (pi-assistant-message-error-message msg))
               (u (pi-assistant-message-usage msg))
               (ts (pi-assistant-message-timestamp msg))
               (prov (pi-assistant-message-provider msg))
               (model (pi-assistant-message-model msg))
               (api (pi-assistant-message-api msg))
               (res (list (cons "role" "assistant") (cons "content" c) (cons "stopReason" sr)))
               (res (if err (append res (list (cons "errorMessage" err))) res))
               (res (if prov (append res (list (cons "provider" prov))) res))
               (res (if model (append res (list (cons "model" model))) res))
               (res (if api (append res (list (cons "api" api))) res))
               (res
                 (if u (append res (list (cons "usage" (%normalize-json-value u)))) res)
               ) ;res
               (res (if ts (append res (list (cons "timestamp" ts))) res))
              ) ;
          res
        ) ;let*
       ) ;
       ((pi-tool-result-message? msg)
        (let* ((c (%normalize-json-value (pi-tool-result-message-content msg)))
               (ts (pi-tool-result-message-timestamp msg))
               (res (list (cons "role" "toolResult")
                      (cons "toolCallId" (pi-tool-result-message-tool-call-id msg))
                      (cons "toolName" (pi-tool-result-message-tool-name msg))
                      (cons "content" c)
                      (cons "isError" (if (pi-tool-result-message-is-error msg) 'true 'false))
                    ) ;list
               ) ;res
              ) ;
          (if ts (append res (list (cons "timestamp" ts))) res)
        ) ;let*
       ) ;
      ) ;cond
    ) ;define

    (define (%json-key->string k)
      (if (symbol? k) (symbol->string k) k)
    ) ;define

    (define (json->pi-message obj)
      (if (pi-message? obj)
        obj
        (begin
          (unless (or (json-object? obj) (null? obj))
            (type-error "json->pi-message: expected json object or pi-message" obj)
          ) ;unless
          (if (null? obj)
            #f
            (let* ((nobj (%normalize-json-keys obj))
                   (role (%json-key->string (json-ref nobj "role")))
                  ) ;
              (cond
               ((equal? role "user")
                (let ((raw-content (json-ref nobj "content"))
                      (raw-ts (json-ref nobj "timestamp"))
                     ) ;
                  (if (null? raw-ts)
                    (pi-user-message (%convert-message-content (if (null? raw-content) "" raw-content)))
                    (pi-user-message (%convert-message-content (if (null? raw-content) "" raw-content))
                      raw-ts
                    ) ;pi-user-message
                  ) ;if
                ) ;let
               ) ;
               ((equal? role "assistant")
                (let ((raw-content (json-ref nobj "content"))
                      (raw-err (json-ref nobj "errorMessage"))
                      (raw-usage (json-ref nobj "usage"))
                      (raw-ts (json-ref nobj "timestamp"))
                      (raw-prov (json-ref nobj "provider"))
                      (raw-model (json-ref nobj "model"))
                      (raw-api (json-ref nobj "api"))
                     ) ;
                  (pi-assistant-message
                    (%convert-message-content (if (null? raw-content) #() raw-content))
                    :stop-reason   (json-ref-string nobj "stopReason" "stop")
                    :error-message (if (string? raw-err) raw-err #f)
                    :usage         (if (null? raw-usage) #f raw-usage)
                    :timestamp     (if (null? raw-ts) (pi-current-time-millis) raw-ts)
                    :provider      (if (string? raw-prov) raw-prov #f)
                    :model         (if (string? raw-model) raw-model #f)
                    :api           (if (string? raw-api) raw-api #f)
                  ) ;pi-assistant-message
                ) ;let
               ) ;
               ((equal? role "toolResult")
                (let ((raw-content (json-ref nobj "content"))
                      (raw-ts (json-ref nobj "timestamp"))
                     ) ;
                  (pi-tool-result-message (json-ref-string nobj "toolCallId" "")
                    (json-ref-string nobj "toolName" "")
                    (%convert-message-content (if (null? raw-content) #() raw-content))
                    (json-ref-boolean nobj "isError" #f)
                    (if (null? raw-ts) (pi-current-time-millis) raw-ts)
                  ) ;pi-tool-result-message
                ) ;let
               ) ;
               (else #f)
              ) ;cond
            ) ;let*
          ) ;if
        ) ;begin
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
