;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi anthropic)
  (import (scheme base) (scheme char) (liii string) (liii json) (liii error))
  (export anthropic-map-stop-reason anthropic-map-usage
    anthropic-build-tool-declarations anthropic-build-messages
    anthropic-build-request-body anthropic-parse-response
    anthropic-is-retriable-error?
  ) ;export
  (begin

    (define (%val key alist default-val)
      (if (not (pair? alist))
        default-val
        (let ((p
                (or (assoc key alist)
                  (assoc (if (symbol? key) (symbol->string key) (string->symbol key)) alist)
                ) ;or
              ) ;p
             ) ;
          (if p (cdr p) default-val)
        ) ;let
      ) ;if
    ) ;define

    (define (anthropic-map-stop-reason reason)
      (let ((r (if (symbol? reason) (symbol->string reason) reason)))
        (cond ((or (equal? r "end_turn") (equal? r "pause_turn") (equal? r "stop_sequence"))
               "stop"
              ) ;
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
          (let ((params (or (%val 'parameters def #f)
                          (%val 'parametersJsonSchema def #f)
                          '(("type" . "object"))
                        ) ;or
                ) ;params
               ) ;
            (list (cons "name" (%val 'name def ""))
              (cons "description" (%val 'description def ""))
              (cons "input_schema" params)
            ) ;list
          ) ;let
        ) ;lambda
        tool-defs
      ) ;map
    ) ;define

    (define (%assistant-blk->anthropic-blk blk)
      (let ((btype (%val 'type blk "")))
        (cond
         ((equal? btype "text")
          (list (cons "type" "text") (cons "text" (%val 'text blk "")))
         ) ;
         ((equal? btype "toolCall")
          (list (cons "type" "tool_use")
            (cons "id" (%val 'id blk ""))
            (cons "name" (%val 'name blk ""))
            (cons "input" (%val 'arguments blk '()))
          ) ;list
         ) ;
         (else (list (cons "type" "text") (cons "text" "")))
        ) ;cond
      ) ;let
    ) ;define

    (define (%tool-result-text content)
      (cond ((string? content) content)
            ((pair? content)
             (let ((first-blk (car content)))
               (if (pair? first-blk) (%val 'text first-blk "") "")
             ) ;let
            ) ;
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
              (let* ((c (%val 'content msg '())) (blks (map %assistant-blk->anthropic-blk c)))
                (loop (cdr rest) (cons (%build-turn-msg "assistant" (list->vector blks)) acc))
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
      (let* ((body (list (cons "model" model-id)
                     (cons "max_tokens" (or max-tokens 8192))
                     (cons "messages" messages)
                   ) ;list
             ) ;body
             (body
               (if
                 (and (string? system-prompt)
                   (not (string-null? (string-trim-both system-prompt)))
                 ) ;and
                 (append body (list (cons "system" system-prompt)))
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

    (define (%extract-blocks raw-content)
      (let* ((content-vec (cond ((vector? raw-content) raw-content)
                                ((list? raw-content) (list->vector raw-content))
                                (else #())
                          ) ;cond
             ) ;content-vec
            ) ;
        (let loop
          ((i 0) (acc '()))
          (if (>= i (vector-length content-vec))
            (reverse acc)
            (let* ((blk (vector-ref content-vec i)) (btype (%val "type" blk "")))
              (cond
               ((equal? btype "text")
                (loop (+ i 1)
                  (cons (list (cons 'type "text") (cons 'text (%val "text" blk ""))) acc)
                ) ;loop
               ) ;
               ((equal? btype "tool_use")
                (loop (+ i 1)
                  (cons (list (cons 'type "toolCall")
                          (cons 'id (%val "id" blk ""))
                          (cons 'name (%val "name" blk ""))
                          (cons 'arguments (%val "input" blk '()))
                        ) ;list
                    acc
                  ) ;cons
                ) ;loop
               ) ;
               (else (loop (+ i 1) acc))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (anthropic-parse-response json-data status-code)
      (if (not (json-object? json-data))
        (list (cons 'role "assistant")
          (cons 'content (list (list (cons 'type "text") (cons 'text ""))))
          (cons 'stopReason "error")
          (cons 'usage (anthropic-map-usage '()))
          (cons 'errorMessage "Invalid JSON response from Anthropic API")
        ) ;list
        (let* ((err (%val "error" json-data #f))
               (raw-stop (%val "stop_reason" json-data "end_turn"))
               (usage (anthropic-map-usage (%val "usage" json-data '())))
              ) ;
          (if (pair? err)
            (list (cons 'role "assistant")
              (cons 'content (list (list (cons 'type "text") (cons 'text ""))))
              (cons 'stopReason "error")
              (cons 'usage usage)
              (cons 'errorMessage (%val "message" err "API error"))
            ) ;list
            (let* ((raw-c (%val "content" json-data #()))
                   (blocks (%extract-blocks raw-c))
                   (stop-reason (anthropic-map-stop-reason raw-stop))
                  ) ;
              (list (cons 'role "assistant")
                (cons 'content blocks)
                (cons 'stopReason stop-reason)
                (cons 'usage usage)
                (cons 'errorMessage (if (equal? stop-reason "error") raw-stop #f))
              ) ;list
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (anthropic-is-retriable-error? status-code response-body-str)
      (cond ((or (= status-code 408)
               (= status-code 409)
               (= status-code 429)
               (= status-code 529)
               (>= status-code 500)
             ) ;or
             #t
            ) ;
            ((zero? status-code) #t)
            (else #f)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
