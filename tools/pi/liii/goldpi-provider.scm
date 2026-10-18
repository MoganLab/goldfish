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

(define-library (liii goldpi-provider)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii json)
    (liii http)
    (liii error)
    (liii time)
    (liii pi)
    (liii goldpi-tools)
  ) ;import
  (export gemini-parse-response gemini-is-retriable-error? gemini-request
    gemini-build-tool-declarations
  ) ;export
  (begin

    (define (%make-tool-call-block name args id ts i)
      (let* ((eff-id
               (if (and (string? id) (not (string-null? id)))
                 id
                 (gemini-generate-tool-call-id name 0 (+ i 1))
               ) ;if
             ) ;eff-id
             (base-blk (list (cons 'type "toolCall")
                         (cons 'id eff-id)
                         (cons 'name name)
                         (cons 'arguments args)
                       ) ;list
             ) ;base-blk
            ) ;
        (if (and (string? ts) (not (string-null? ts)))
          (append base-blk (list (cons 'thoughtSignature ts)))
          base-blk
        ) ;if
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
                (loop (+ i 1) (cons (list (cons 'type "text") (cons 'text t)) acc) has-fn?)
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
        (list (cons 'role "assistant")
          (cons 'content (list (list (cons 'type "text") (cons 'text ""))))
          (cons 'stopReason "error")
          (cons 'usage (gemini-map-usage '()))
          (cons 'errorMessage "Invalid JSON response from Gemini API")
        ) ;list
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
              (list (cons 'role "assistant")
                (cons 'content (list (list (cons 'type "text") (cons 'text ""))))
                (cons 'stopReason "error")
                (cons 'usage usage)
                (cons 'errorMessage msg)
              ) ;list
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
                  (list (cons 'role "assistant")
                    (cons 'content blocks)
                    (cons 'stopReason stop-reason)
                    (cons 'usage usage)
                    (cons 'errorMessage err-msg)
                  ) ;list
                ) ;let*
              ) ;let-values
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (gemini-is-retriable-error? status-code response-body-str)
      (or (= status-code 408)
        (= status-code 409)
        (= status-code 429)
        (>= status-code 500)
        (zero? status-code)
      ) ;or
    ) ;define

    (define (gemini-build-tool-declarations active-tool-names)
      (let loop
        ((names active-tool-names) (acc '()))
        (if (null? names)
          (reverse acc)
          (let* ((name (car names)) (def (get-tool-definition name)))
            (if def
              (let* ((desc (cdr (assoc 'description def)))
                     (params (cdr (assoc 'parameters def)))
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

    (define (%safe-assoc key alist)
      (if (and (pair? alist) (every pair? alist)) (assoc key alist) #f)
    ) ;define

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
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
             (retry-cfg
               (or opt-retry (cdr (or (%safe-assoc 'retry settings) '(#f))))
             ) ;retry-cfg
             (retry-enabled? (cdr (or (%safe-assoc 'enabled retry-cfg) '(#f
                                                                         . #t))))
             (max-retries
               (if retry-enabled? (cdr (or (%safe-assoc 'max-retries retry-cfg) '(#f
                                                                                  . 3))) 0)
             ) ;max-retries
             (base-delay-ms (cdr (or (%safe-assoc 'base-delay-ms retry-cfg) '(#f
                                                                              . 2000))))
             (max-agent-delay-ms (cdr (or (%safe-assoc 'max-agent-delay-ms retry-cfg) '(#f
                                                                                        . 60000)))
             ) ;max-agent-delay-ms
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
             (req-body (gemini-build-request-body contents system-instruction tool-decls))
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
        (let loop
          ((attempt 1))
          (let* ((resp
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
                       ;; 网络异常等
                       (let ((err-msg (if (pair? args) (car args) "Network error")))
                         (list (cons 'status-code 0) (cons 'reason err-msg) (cons 'text ""))
                       ) ;let
                     ) ;lambda
                   ) ;catch
                 ) ;resp
                 (status (or (%resp-field resp 'status-code) 0))
                 (body-str (or (%resp-field resp 'text) ""))
                ) ;
            (if (gemini-is-retriable-error? status body-str)
              (if (<= attempt max-retries)
                (let* ((calc-delay (* base-delay-ms (expt 2 (- attempt 1))))
                       (delay-ms (min calc-delay max-agent-delay-ms))
                       (delay-sec (/ delay-ms 1000.0))
                      ) ;
                  (sleep delay-sec)
                  (loop (+ attempt 1))
                ) ;let*
                ;; 超出重试次数
                (list (cons 'role "assistant")
                  (cons 'content (list (list (cons 'type "text") (cons 'text ""))))
                  (cons 'stopReason "error")
                  (cons 'usage (gemini-map-usage '()))
                  (cons 'errorMessage
                    (string-append "HTTP "
                      (number->string status)
                      " after "
                      (number->string attempt)
                      " attempts: "
                      body-str
                    ) ;string-append
                  ) ;cons
                ) ;list
              ) ;if
              ;; 成功或非重试错误
              (let ((parsed-json (catch #t (lambda () (string->json body-str)) (lambda args #f))))
                (gemini-parse-response parsed-json status)
              ) ;let
            ) ;if
          ) ;let*
        ) ;let
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
