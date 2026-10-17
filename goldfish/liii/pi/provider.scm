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

(define-library (liii pi provider)
  (import (scheme base) (scheme char) (liii string) (liii json) (liii error))
  (export gemini-map-finish-reason gemini-map-usage
    gemini-normalize-tool-call-id gemini-generate-tool-call-id
    gemini-build-contents gemini-build-request-body
  ) ;export
  (begin

    (define (gemini-map-finish-reason finish-reason has-function-call?)
      (cond ((or (equal? finish-reason "STOP") (eq? finish-reason 'STOP))
             (if has-function-call? "toolUse" "stop")
            ) ;
            ((or (equal? finish-reason "MAX_TOKENS") (eq? finish-reason 'MAX_TOKENS))
             "length"
            ) ;
            (else "error")
      ) ;cond
    ) ;define

    (define (%usage-field usage-meta key sym-key)
      (if (not (pair? usage-meta))
        0
        (let ((pair (or (assoc key usage-meta) (assoc sym-key usage-meta))))
          (if pair (cdr pair) 0)
        ) ;let
      ) ;if
    ) ;define

    (define (gemini-map-usage usage-meta)
      (let* ((prompt-tokens (%usage-field usage-meta "promptTokenCount" 'promptTokenCount))
             (candidates-tokens (%usage-field usage-meta "candidatesTokenCount" 'candidatesTokenCount)
             ) ;candidates-tokens
             (thoughts-tokens (%usage-field usage-meta "thoughtsTokenCount" 'thoughtsTokenCount)
             ) ;thoughts-tokens
             (cached-tokens (%usage-field usage-meta "cachedContentTokenCount" 'cachedContentTokenCount)
             ) ;cached-tokens
             (total-tokens (%usage-field usage-meta "totalTokenCount" 'totalTokenCount))
             (input-tokens (- prompt-tokens cached-tokens))
             (output-tokens (+ candidates-tokens thoughts-tokens))
            ) ;
        (list (cons 'input input-tokens)
          (cons 'output output-tokens)
          (cons 'cacheRead cached-tokens)
          (cons 'cacheWrite 0)
          (cons 'totalTokens total-tokens)
        ) ;list
      ) ;let*
    ) ;define

    (define (%map-char c)
      (if (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\-))
        c
        #\_
      ) ;if
    ) ;define

    (define (gemini-normalize-tool-call-id id)
      (if (not (string? id))
        "tool_call_0"
        (let* ((chars (string->list id))
               (mapped (map %map-char chars))
               (res (list->string mapped))
              ) ;
          (if (> (string-length res) 64) (substring res 0 64) res)
        ) ;let*
      ) ;if
    ) ;define

    (define (gemini-generate-tool-call-id name timestamp counter)
      (let ((raw (string-append name "_" (number->string timestamp) "_" (number->string counter))
            ) ;raw
           ) ;
        (gemini-normalize-tool-call-id raw)
      ) ;let
    ) ;define

    (define (%assistant-block->part blk)
      (let ((btype (cdr (assoc 'type blk))))
        (cond
         ((equal? btype "text") (list (cons "text" (cdr (assoc 'text blk)))))
         ((equal? btype "toolCall")
          (list
            (cons "functionCall"
              (list (cons "name" (cdr (assoc 'name blk)))
                (cons "args" (cdr (assoc 'arguments blk)))
                (cons "id" (cdr (assoc 'id blk)))
              ) ;list
            ) ;cons
          ) ;list
         ) ;
         (else (list (cons "text" "")))
        ) ;cond
      ) ;let
    ) ;define

    (define (%build-single-content msg)
      (let ((role (cdr (assoc 'role msg))) (content (cdr (assoc 'content msg))))
        (cond
         ((equal? role "user")
          (list (cons "role" "user") (cons "parts" (vector (list (cons "text" content)))))
         ) ;
         ((equal? role "assistant")
          (let ((parts (map %assistant-block->part content)))
            (list (cons "role" "model") (cons "parts" (list->vector parts)))
          ) ;let
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%tool-result-text content)
      (if (not (pair? content))
        ""
        (let ((first-blk (car content)))
          (if (not (pair? first-blk))
            ""
            (cdr (or (assoc 'text first-blk) (assoc "text" first-blk) '(#f . "")))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%tool-result->function-response msg)
      (let* ((tool-name (cdr (assoc 'toolName msg)))
             (tool-id (cdr (assoc 'toolCallId msg)))
             (is-err? (cdr (assoc 'isError msg)))
             (text (%tool-result-text (cdr (assoc 'content msg))))
             (resp-obj (if is-err? (list (cons "error" text)) (list (cons "output" text))))
            ) ;
        (list (cons "functionResponse"
                (list (cons "name" tool-name) (cons "response" resp-obj) (cons "id" tool-id))
              ) ;cons
        ) ;list
      ) ;let*
    ) ;define

    (define (%take-tool-results rest parts-acc)
      (if
        (and (pair? rest) (equal? (cdr (assoc 'role (car rest))) "toolResult"))
        (%take-tool-results (cdr rest)
          (cons (%tool-result->function-response (car rest)) parts-acc)
        ) ;%take-tool-results
        (values rest parts-acc)
      ) ;if
    ) ;define

    (define (gemini-build-contents messages)
      (let loop
        ((rest messages) (acc '()))
        (if (null? rest)
          (list->vector (reverse acc))
          (let* ((msg (car rest)) (role (cdr (assoc 'role msg))))
            (if (equal? role "toolResult")
              (let*-values (((rem-rest rparts) (%take-tool-results rest '()))
                            ((parts-vec) (list->vector (reverse rparts)))
                            ((merged-turn) (list (cons "role" "user") (cons "parts" parts-vec)))
                           ) ;
                (loop rem-rest (cons merged-turn acc))
              ) ;let*-values
              (let ((c (%build-single-content msg)))
                (loop (cdr rest) (if c (cons c acc) acc))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (gemini-build-request-body contents system-instruction tool-declarations)
      (let* ((req '())
             (req (cons (cons "contents" contents) req))
             (req
               (if
                 (and (string? system-instruction)
                   (not (string-null? (string-trim-both system-instruction)))
                 ) ;and
                 (cons (cons "systemInstruction" system-instruction) req)
                 req
               ) ;if
             ) ;req
             (req
               (if (and (pair? tool-declarations) (> (length tool-declarations) 0))
                 (let ((tools-val
                         (vector (list (cons "functionDeclarations" (list->vector tool-declarations))))
                       ) ;tools-val
                       (tc-val
                         (list (cons "functionCallingConfig" (list (cons "mode" "AUTO"))))
                       ) ;tc-val
                      ) ;
                   (cons (cons "toolConfig" tc-val) (cons (cons "tools" tools-val) req))
                 ) ;let
                 req
               ) ;if
             ) ;req
            ) ;
        (reverse req)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
