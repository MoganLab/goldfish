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
  (import (scheme base)
    (scheme char)
    (liii list)
    (liii string)
    (liii json)
    (liii error)
  ) ;import
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
             (cand-tokens (%usage-field usage-meta "candidatesTokenCount" 'candidatesTokenCount)
             ) ;cand-tokens
             (thought-tokens (%usage-field usage-meta "thoughtsTokenCount" 'thoughtsTokenCount)
             ) ;thought-tokens
             (cached-tokens (%usage-field usage-meta "cachedContentTokenCount" 'cachedContentTokenCount)
             ) ;cached-tokens
             (total-tokens (%usage-field usage-meta "totalTokenCount" 'totalTokenCount))
             (in-tokens (- prompt-tokens cached-tokens))
             (out-tokens (+ cand-tokens thought-tokens))
            ) ;
        (list (cons 'input in-tokens)
          (cons 'output out-tokens)
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
        (let ((res (list->string (map %map-char (string->list id)))))
          (if (> (string-length res) 64) (substring res 0 64) res)
        ) ;let
      ) ;if
    ) ;define

    (define (gemini-generate-tool-call-id name timestamp counter)
      (let ((raw (string-append name "_" (number->string timestamp) "_" (number->string counter))
            ) ;raw
           ) ;
        (gemini-normalize-tool-call-id raw)
      ) ;let
    ) ;define

    (define* (%val key alist (default-val #f))
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
    ) ;define*

    (define (%assistant-block->part blk)
      (let ((btype (%val 'type blk "")))
        (cond
         ((equal? btype "text")
          (let ((t (%val 'text blk "")))
            (if (or (not (string? t)) (string-null? (string-trim-both t)))
              #f
              (list (cons "text" t))
            ) ;if
          ) ;let
         ) ;
         ((equal? btype "toolCall")
          (let* ((ts (%val 'thoughtSignature blk #f))
                 (fc-obj (list (cons "name" (%val 'name blk ""))
                           (cons "args" (%val 'arguments blk '()))
                           (cons "id" (%val 'id blk ""))
                         ) ;list
                 ) ;fc-obj
                 (part (list (cons "functionCall" fc-obj)))
                ) ;
            (if (and (string? ts) (not (string-null? ts)))
              (cons (cons "thoughtSignature" ts) part)
              part
            ) ;if
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%extract-user-text-content content)
      (cond ((string? content) content)
            ((vector? content)
             (let loop
               ((i 0) (acc '()))
               (if (>= i (vector-length content))
                 (string-join (reverse acc) "\n")
                 (let* ((blk (vector-ref content i)) (t (%val 'text blk "")))
                   (loop (+ i 1) (if (string-null? t) acc (cons t acc)))
                 ) ;let*
               ) ;if
             ) ;let
            ) ;
            ((pair? content)
             (let loop
               ((rest content) (acc '()))
               (if (null? rest)
                 (string-join (reverse acc) "\n")
                 (let* ((blk (car rest)) (t (if (pair? blk) (%val 'text blk "") "")))
                   (loop (cdr rest) (if (string-null? t) acc (cons t acc)))
                 ) ;let*
               ) ;if
             ) ;let
            ) ;
            (else "")
      ) ;cond
    ) ;define

    (define (%build-single-content msg)
      (let ((role (%val 'role msg "")) (content (%val 'content msg '())))
        (cond
         ((equal? role "user")
          (let ((text-content (%extract-user-text-content content)))
            (list (cons "role" "user")
              (cons "parts" (vector (list (cons "text" text-content))))
            ) ;list
          ) ;let
         ) ;
         ((equal? role "assistant")
          (let* ((blks (cond ((vector? content) (vector->list content))
                             ((list? content) content)
                             (else '())
                       ) ;cond
                 ) ;blks
                 (parts
                   (filter
                     (lambda (p) (and p (not (null? p))))
                     (map %assistant-block->part blks)
                   ) ;filter
                 ) ;parts
                ) ;
            (if (null? parts)
              #f
              (list (cons "role" "model") (cons "parts" (list->vector parts)))
            ) ;if
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%tool-result-text content)
      (cond ((string? content) content)
            ((vector? content)
             (if (> (vector-length content) 0) (%val 'text (vector-ref content 0) "") "")
            ) ;
            ((pair? content)
             (let ((first-blk (car content)))
               (if (pair? first-blk) (%val 'text first-blk "") "")
             ) ;let
            ) ;
            (else "")
      ) ;cond
    ) ;define

    (define (%tool-result->function-response msg)
      (let* ((tool-name (%val 'toolName msg ""))
             (tool-id (%val 'toolCallId msg ""))
             (is-err? (%val 'isError msg #f))
             (text (%tool-result-text (%val 'content msg '())))
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
        (and (pair? rest) (equal? (%val 'role (car rest) "") "toolResult"))
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
          (let* ((msg (car rest)) (role (%val 'role msg "")))
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

    (define (gemini-build-request-body contents sys-inst tool-decls)
      (let* ((req (list (cons "contents" contents)))
             (req
               (if
                 (and (string? sys-inst) (not (string-null? (string-trim-both sys-inst))))
                 (cons
                   (cons "systemInstruction"
                     (list (cons "role" "user")
                       (cons "parts" (vector (list (cons "text" sys-inst))))
                     ) ;list
                   ) ;cons
                   req
                 ) ;cons
                 req
               ) ;if
             ) ;req
             (req
               (if (and (pair? tool-decls) (> (length tool-decls) 0))
                 (let ((tools-val
                         (vector (list (cons "functionDeclarations" (list->vector tool-decls))))
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
