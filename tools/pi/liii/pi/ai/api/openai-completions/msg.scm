;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api openai-completions msg)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii pi ai message)
    (liii pi ai event)
    (liii pi ai tools types)
    (liii pi ai tools record)
  ) ;import
  (export openai-build-tool-declarations
    openai-build-messages
    openai-build-request-body
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

    (define (%msg-text-content content)
      (cond ((string? content) content)
            ((vector? content) (%msg-text-content (vector->list content)))
            ((pair? content)
             (let loop
               ((lst content) (acc ""))
               (if (null? lst)
                 acc
                 (let* ((blk (car lst))
                        (t (cond ((string? blk) blk)
                                 ((pi-text-block? blk) (pi-text-block-text blk))
                                 ((pair? blk) (%val 'text blk ""))
                                 (else "")
                           ) ;cond
                        ) ;t
                       ) ;
                   (loop (cdr lst) (string-append acc t))
                 ) ;let*
               ) ;if
             ) ;let
            ) ;
            (else "")
      ) ;cond
    ) ;define

    (define (%convert-assistant-msg msg)
      (let* ((raw-c (%val 'content msg '()))
             (c-list (cond ((vector? raw-c) (vector->list raw-c))
                           ((pair? raw-c) raw-c)
                           (else '())
                     ) ;cond
             ) ;c-list
             (text-acc "")
             (tc-acc '())
            ) ;
        (for-each
          (lambda (blk)
            (cond
             ((or (pi-text-block? blk) (equal? (%val 'type blk "") "text"))
              (set! text-acc
                (string-append text-acc
                  (if (pi-text-block? blk) (pi-text-block-text blk) (%val 'text blk ""))
                ) ;string-append
              ) ;set!
             ) ;
             ((or (pi-tool-call-block? blk) (equal? (%val 'type blk "") "toolCall"))
              (let* ((id (if (pi-tool-call-block? blk) (pi-tool-call-block-id blk) (%val 'id blk ""))
                     ) ;id
                     (name (if (pi-tool-call-block? blk) (pi-tool-call-block-name blk) (%val 'name blk ""))
                     ) ;name
                     (raw-args (if (pi-tool-call-block? blk)
                                 (pi-tool-call-block-arguments blk)
                                 (%val 'arguments blk '())
                               ) ;if
                     ) ;raw-args
                     (args-str (if (string? raw-args) raw-args (json->string raw-args)))
                     (tc
                       (list (cons "id" id)
                         (cons "type" "function")
                         (cons "function" (list (cons "name" name) (cons "arguments" args-str)))
                       ) ;list
                     ) ;tc
                    ) ;
                (set! tc-acc (cons tc tc-acc))
              ) ;let*
             ) ;
            ) ;cond
          ) ;lambda
          c-list
        ) ;for-each
        (if (pair? tc-acc)
          (list (cons "role" "assistant")
            (cons "content" (if (string-null? text-acc) "" text-acc))
            (cons "tool_calls" (list->vector (reverse tc-acc)))
          ) ;list
          (list (cons "role" "assistant") (cons "content" text-acc))
        ) ;if
      ) ;let*
    ) ;define

    (define (openai-build-messages messages system-instruction)
      (let* ((acc '())
             (acc
               (if
                 (and (string? system-instruction)
                   (not (string-null? (string-trim-both system-instruction)))
                 ) ;and
                 (cons (list (cons "role" "system") (cons "content" system-instruction)) acc)
                 acc
               ) ;if
             ) ;acc
            ) ;
        (for-each
          (lambda (msg)
            (let ((role (%val 'role msg "")))
              (cond
               ((equal? role "user")
                (set! acc
                  (cons
                    (list (cons "role" "user")
                      (cons "content" (%msg-text-content (%val 'content msg "")))
                    ) ;list
                    acc
                  ) ;cons
                ) ;set!
               ) ;
               ((equal? role "assistant") (set! acc (cons (%convert-assistant-msg msg) acc)))
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
                  (set! acc
                    (cons (list (cons "role" "tool") (cons "tool_call_id" call-id) (cons "content" text))
                      acc
                    ) ;cons
                  ) ;set!
                ) ;let
               ) ;
              ) ;cond
            ) ;let
          ) ;lambda
          messages
        ) ;for-each
        (list->vector (reverse acc))
      ) ;let*
    ) ;define

    (define (openai-build-request-body model-id messages stream? tool-decls max-tokens)
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
            ) ;
        body
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
