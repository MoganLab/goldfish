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

(define-library (liii pi gemini request)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii json)
    (liii http)
    (liii error)
    (liii time)
    (liii pi retry)
    (liii pi tools)
    (liii pi message)
    (liii pi gemini core)
    (liii pi gemini stream-parse)
    (liii pi gemini thinking)
  ) ;import
  (export gemini-parse-response gemini-is-retriable-error? gemini-request
    gemini-build-tool-declarations format-failed-stream-msg
  ) ;export
  (begin

    (define (%make-error-response msg usage)
      (pi-assistant-message (list (pi-text-block ""))
        :stop-reason   "error"
        :error-message msg
        :usage         usage
      ) ;pi-assistant-message
    ) ;define

    (define (%make-tool-call-block name args id ts i)
      (let* ((eff-id
               (if (and (string? id) (not (string-null? id)))
                 id
                 (gemini-generate-tool-call-id)
               ) ;if
             ) ;eff-id
            ) ;
        (pi-tool-call-block eff-id name args :thought-signature ts)
      ) ;let*
    ) ;define

    (define (%extract-parts-content parts)
      (let loop
        ((i 0) (acc '()) (has-fn? #f))
        (if (>= i (vector-length parts))
          (values (reverse acc) has-fn?)
          (let ((part (vector-ref parts i)))
            (cond
             ((assoc "text" part)
              (let ((t (cdr (assoc "text" part))))
                (cond
                 ((gemini-is-thinking-part? part)
                  (let ((ts (gemini-part-thought-signature part)))
                    (loop (+ i 1) (cons (pi-thinking-block t ts) acc) has-fn?)
                  ) ;let
                 ) ;
                 ((and (string? t) (not (string-null? t)))
                  (loop (+ i 1) (cons (pi-text-block t) acc) has-fn?)
                 ) ;
                 (else (loop (+ i 1) acc has-fn?))
                ) ;cond
              ) ;let
             ) ;
             ((assoc "functionCall" part)
              (let* ((fc (cdr (assoc "functionCall" part)))
                     (name (cdr (assoc "name" fc)))
                     (args (cdr (assoc "args" fc)))
                     (id (cdr (or (assoc "id" fc) (assoc 'id fc) '(#f . #f))))
                     (ts (cdr (or (assoc "thoughtSignature" part)
                                (assoc 'thoughtSignature part)
                                (assoc "thoughtSignature" fc)
                                (assoc 'thoughtSignature fc)
                                '(#f . #f)
                              ) ;or
                         ) ;cdr
                     ) ;ts
                     (tc-blk (%make-tool-call-block name args id ts i))
                    ) ;
                (loop (+ i 1) (cons tc-blk acc) #t)
              ) ;let*
             ) ;
             (else (loop (+ i 1) acc has-fn?))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (gemini-parse-response json-data status-code)
      (if (not (json-object? json-data))
        (%make-error-response "Invalid JSON response from Gemini API"
          (gemini-map-usage '())
        ) ;%make-error-response
        (let* ((candidates (cdr (or (assoc "candidates" json-data) '(#f . #f))))
               (usage-meta (cdr (or (assoc "usageMetadata" json-data) '(#f . #f))))
               (usage (gemini-map-usage usage-meta))
              ) ;
          (if (or (not (vector? candidates)) (zero? (vector-length candidates)))
            (let* ((err (cdr (or (assoc "error" json-data) '(#f . #f))))
                   (msg
                     (if (pair? err)
                       (cdr (or (assoc "message" err) '(#f . "API error")))
                       "No candidate returned"
                     ) ;if
                   ) ;msg
                  ) ;
              (%make-error-response msg usage)
            ) ;let*
            (let* ((candidate (vector-ref candidates 0))
                   (content-obj (cdr (or (assoc "content" candidate) '(#f
                                                                       #_quote
                                                                       ()))))
                   (parts (cdr (or (assoc "parts" content-obj) '(#f . #()))))
                   (finish-reason (cdr (or (assoc "finishReason" candidate) '(#f
                                                                              . "STOP"))))
                  ) ;
              (let-values (((blocks has-fn?) (%extract-parts-content parts)))
                (let* ((stop-reason (gemini-map-finish-reason finish-reason has-fn?))
                       (err-msg (if (equal? stop-reason "error") finish-reason #f))
                      ) ;
                  (pi-assistant-message blocks
                    :stop-reason   stop-reason
                    :error-message err-msg
                    :usage         usage
                  ) ;pi-assistant-message
                ) ;let*
              ) ;let-values
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (gemini-build-tool-declarations active-tool-names)
      (let loop
        ((names active-tool-names) (acc '()))
        (if (null? names)
          (reverse acc)
          (let* ((name (car names)) (def (get-tool-definition name)))
            (if def
              (let* ((desc (if (pi-tool-definition? def)
                             (pi-tool-definition-description def)
                             (cdr (assoc 'description def))
                           ) ;if
                     ) ;desc
                     (params (if (pi-tool-definition? def)
                               (pi-tool-definition-parameters def)
                               (cdr (assoc 'parameters def))
                             ) ;if
                     ) ;params
                     (decl (list (cons "name" name)
                             (cons "description" desc)
                             (cons "parametersJsonSchema" params)
                           ) ;list
                     ) ;decl
                    ) ;
                (loop (cdr names) (cons decl acc))
              ) ;let*
              (loop (cdr names) acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (%http-post-safe url req-json-str headers proxy-alist timeout)
      (catch #t
        (lambda ()
          (http-post url
            :data    req-json-str
            :headers headers
            :proxy   proxy-alist
            :timeout timeout
          ) ;http-post
        ) ;lambda
        (lambda (tag . args)
          (let ((err-msg (if (pair? args) (car args) "Network error")))
            (list (cons 'status-code 0) (cons 'reason err-msg) (cons 'text ""))
          ) ;let
        ) ;lambda
      ) ;catch
    ) ;define

    (define* (gemini-request config
               messages
               (system-instruction #f)
               (tool-names #f)
               (timeout 300)
               (opt-retry #f)
             ) ;gemini-request
      (let* ((provider (cdr (assoc 'provider config)))
             (api-key (cdr (assoc 'api-key config)))
             (model-id (cdr (assoc 'model-id config)))
             (model-meta (cdr (assoc 'model-meta config)))
             (proxy-url (cdr (assoc 'proxy-url config)))
             (settings (cdr (assoc 'settings config)))
             (base-url (cdr (or (assoc 'base-url model-meta)
                              '(#f
                                . "https://generativelanguage.googleapis.com/v1beta")
                            ) ;or
                       ) ;cdr
             ) ;base-url
             (url (string-append base-url "/models/" model-id ":generateContent"))
             (eff-tool-names (or tool-names (cdr (assoc 'tools config))))
             (tool-decls (gemini-build-tool-declarations eff-tool-names))
             (contents (gemini-build-contents messages))
             (raw-req-body (gemini-build-request-body contents system-instruction tool-decls)
             ) ;raw-req-body
             (thinking-lvl (cdr (or (assoc 'thinking-level config) '(#f . #f))))
             (req-body (gemini-inject-thinking-config raw-req-body thinking-lvl))
             (req-json-str (json->string req-body))
             (headers (list (cons "Content-Type" "application/json")
                        (cons "x-goog-api-key" (or api-key ""))
                      ) ;list
             ) ;headers
             (proxy-alist
               (if (and (string? proxy-url) (not (string-null? proxy-url)))
                 (list (cons "http" proxy-url) (cons "https" proxy-url))
                 '()
               ) ;if
             ) ;proxy-alist
            ) ;
        (let-values (((max-retries base-delay-ms max-agent-delay-ms)
                      (pi-resolve-retry-params settings opt-retry)
                     ) ;
                    ) ;
          (let loop
            ((attempt 1))
            (let* ((resp (%http-post-safe url req-json-str headers proxy-alist timeout))
                   (status (or (%resp-field resp 'status-code) 0))
                   (body-str (or (%resp-field resp 'text) ""))
                  ) ;
              (cond ((and (gemini-is-retriable-error? status body-str) (<= attempt max-retries))
                     (sleep (pi-calc-retry-delay attempt base-delay-ms max-agent-delay-ms))
                     (loop (+ attempt 1))
                    ) ;
                    ((gemini-is-retriable-error? status body-str)
                     (%make-error-response (format-failed-stream-msg status resp)
                       (gemini-map-usage '())
                     ) ;%make-error-response
                    ) ;
                    (else
                      (let ((pj (catch #t (lambda () (string->json body-str)) (lambda _ #f))))
                        (gemini-parse-response pj status)
                      ) ;let
                    ) ;else
              ) ;cond
            ) ;let*
          ) ;let
        ) ;let-values
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
