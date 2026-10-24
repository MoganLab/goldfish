;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code tools web-search execute)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii json)
    (liii http)
    (liii error)
    (liii pi code tools read)
    (liii pi ai gemini grounding)
    (liii pi ai gemini request)
    (liii pi code tools web-search schema)
    (liii pi code tools web-search config)
    (liii pi code tools web-search deepseek)
    (liii pi code tools web-search format)
  ) ;import
  (export pi-execute-web-search)
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (%build-search-request-body query urls)
      (let* ((has-urls? (and (pair? urls) (not (null? urls))))
             (prompt (if has-urls?
                       (string-append query "\n\nAlso analyze these URLs:\n" (string-join urls "\n"))
                       query
                     ) ;if
             ) ;prompt
             (contents
               (vector
                 (list (cons "role" "user") (cons "parts" (vector (list (cons "text" prompt)))))
               ) ;vector
             ) ;contents
             (tools (gemini-build-search-tools '()))
            ) ;
        (list (cons "contents" contents) (cons "tools" tools))
      ) ;let*
    ) ;define

    (define (%extract-answer-text json-data)
      (let ((cands (cdr (or (assoc "candidates" json-data) '(#f . #f)))))
        (if (or (not (vector? cands)) (zero? (vector-length cands)))
          ""
          (let* ((c0 (vector-ref cands 0))
                 (content-obj (cdr (or (assoc "content" c0) '(#f . #f))))
                 (parts
                   (if (pair? content-obj) (cdr (or (assoc "parts" content-obj) '(#f
                                                                                  . #()))) #())
                 ) ;parts
                ) ;
            (string-join
              (filter string?
                (map
                  (lambda (p) (cdr (or (assoc "text" p) '(#f . #f))))
                  (vector->list parts)
                ) ;map
              ) ;filter
              ""
            ) ;string-join
          ) ;let*
        ) ;if
      ) ;let
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

    (define (%handle-success-response body-str model-id)
      (let ((json-data (catch #t (lambda () (string->json body-str)) (lambda _ #f))))
        (unless (pair? json-data)
          (error 'error "web_search: Invalid JSON response from Gemini API")
        ) ;unless
        (let ((cands (cdr (or (assoc "candidates" json-data) '(#f . #f)))))
          (if (or (not (vector? cands)) (zero? (vector-length cands)))
            (pi-format-web-search-result "No search results or candidates returned"
              '()
              :model-id model-id
            ) ;pi-format-web-search-result
            (let ((ans (or (%extract-answer-text json-data) "")))
              (pi-format-web-search-result ans json-data :model-id model-id)
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%handle-response status body-str model-id)
      (cond ((= status 0)
             (error 'error
               "web_search network error: request timed out or connection failed"
             ) ;error
            ) ;
            ((or (= status 401) (= status 403))
             (error 'error
               (string-append "web_search authentication failed (HTTP "
                 (number->string status)
                 "): "
                 body-str
               ) ;string-append
             ) ;error
            ) ;
            ((>= status 400)
             (error 'error
               (string-append "web_search API error (HTTP "
                 (number->string status)
                 "): "
                 body-str
               ) ;string-append
             ) ;error
            ) ;
            (else (%handle-success-response body-str model-id))
      ) ;cond
    ) ;define

    (define (%handle-deepseek-success-response body-str)
      (let ((json-data (catch #t (lambda () (string->json body-str)) (lambda _ #f))))
        (unless (pair? json-data)
          (error 'error "web_search: Invalid JSON response from DeepSeek API")
        ) ;unless
        (let ((results (deepseek-parse-search-results json-data)))
          (if (null? results)
            (pi-format-web-search-result "No search results returned"
              '()
              :model-id "deepseek-search"
            ) ;pi-format-web-search-result
            (let ((ans (deepseek-format-search-text results)))
              (pi-format-web-search-result ans
                '()
                :model-id "deepseek-search"
                :sources  (deepseek-sources-from-results results)
              ) ;pi-format-web-search-result
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%handle-deepseek-response status body-str)
      (cond ((= status 0)
             (error 'error
               "web_search network error: request timed out or connection failed"
             ) ;error
            ) ;
            ((or (= status 401) (= status 403))
             (error 'error
               (string-append "web_search authentication failed (HTTP "
                 (number->string status)
                 "): "
                 body-str
               ) ;string-append
             ) ;error
            ) ;
            ((>= status 400)
             (error 'error
               (string-append "web_search API error (HTTP "
                 (number->string status)
                 "): "
                 body-str
               ) ;string-append
             ) ;error
            ) ;
            (else (%handle-deepseek-success-response body-str))
      ) ;cond
    ) ;define

    (define (%deepseek-fetch-pages urls base-url api-key proxy-alist timeout)
      (let loop
        ((rem (if (vector? urls) (vector->list urls) urls)) (text ""))
        (if (null? rem)
          text
          (let* ((url (car rem))
                 (req-json-str (json->string (deepseek-build-fetch-request-body url)))
                 (endpoint (deepseek-build-fetch-endpoint base-url))
                 (headers (list (cons "Content-Type" "application/json")
                            (cons "Authorization" (string-append "Bearer " api-key))
                          ) ;list
                 ) ;headers
                 (resp (%http-post-safe endpoint req-json-str headers proxy-alist timeout))
                 (status (or (%resp-field resp 'status-code) 0))
                 (body-str (or (%resp-field resp 'text) ""))
                ) ;
            (if (>= status 400)
              ;; 单个页面拉取失败不中断整体搜索结果
              (loop (cdr rem) text)
              (let* ((json-data (catch #t (lambda () (string->json body-str)) (lambda _ #f)))
                     (page
                       (if (pair? json-data)
                         (append (deepseek-parse-fetch-page json-data) (list (cons "url" url)))
                         (list (cons "url" url))
                       ) ;if
                     ) ;page
                    ) ;
                (loop (cdr rem) (deepseek-append-fetched-page text page))
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (pi-execute-deepseek-web-search query urls api-key base-url proxy-alist timeout)
      (let* ((req-body (deepseek-build-search-request-body query))
             (req-json-str (json->string req-body))
             (endpoint (deepseek-build-search-endpoint base-url))
             (headers (list (cons "Content-Type" "application/json")
                        (cons "Authorization" (string-append "Bearer " api-key))
                      ) ;list
             ) ;headers
             (resp (%http-post-safe endpoint req-json-str headers proxy-alist timeout))
             (status (or (%resp-field resp 'status-code) 0))
             (body-str (or (%resp-field resp 'text) ""))
             (res (%handle-deepseek-response status body-str))
             (url-list (cond ((vector? urls) (vector->list urls))
                             ((pair? urls) urls)
                             (else '())
                       ) ;cond
             ) ;url-list
            ) ;
        (if (null? url-list)
          res
          (let* ((fetched (%deepseek-fetch-pages url-list base-url api-key proxy-alist timeout))
                 (orig-text (cdr (assoc 'text res)))
                 (new-text (if (string-null? (string-trim-both orig-text))
                             fetched
                             (string-append orig-text "\n\n" fetched)
                           ) ;if
                 ) ;new-text
                ) ;
            (list
              (cons 'content (vector (list (cons 'type "text") (cons 'text new-text))))
              (cons 'text new-text)
              (cons 'sources (cdr (assoc 'sources res)))
              (cons 'details (cdr (assoc 'details res)))
              (cons 'isError #f)
            ) ;list
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-execute-web-search args . opts)
      (let* ((timed-out? (pi-keyword-value :timed-out? opts #f)))
        (when timed-out?
          (error 'error "Command timed out")
        ) ;when
        (pi-validate-web-search-args args)
        (let* ((config (pi-keyword-value :config opts '()))
               (opt-env (pi-keyword-value :env opts #f))
               (ws-config (if (pair? opt-env)
                            (pi-load-web-search-config opt-env)
                            (pi-load-web-search-config)
                          ) ;if
               ) ;ws-config
               (opt-provider (pi-keyword-value :provider opts #f))
               (provider (if (pair? opt-env)
                           (pi-resolve-web-search-provider config opt-provider ws-config opt-env)
                           (pi-resolve-web-search-provider config opt-provider ws-config)
                         ) ;if
               ) ;provider
               (opt-key (pi-keyword-value :api-key opts #f))
               (opt-url (pi-keyword-value :base-url opts #f))
               (opt-proxy (pi-keyword-value :proxy-url opts #f))
               (proxy-url (pi-resolve-search-proxy-url config opt-proxy))
               (proxy-alist (pi-resolve-search-proxy-alist proxy-url))
               (timeout (pi-keyword-value :timeout opts 600))
               (query (pi-arg-ref args "query"))
               (urls (pi-arg-ref args "urls" '()))
              ) ;
          (if (equal? provider "deepseek")
            ;; DeepSeek 后端：独立 Search / Fetch API
            (let* ((api-key (if (pair? opt-env)
                              (pi-resolve-deepseek-api-key config opt-key opt-env)
                              (pi-resolve-deepseek-api-key config opt-key)
                            ) ;if
                   ) ;api-key
                   (base-url (if (pair? opt-env)
                               (pi-resolve-deepseek-search-base-url config opt-url opt-env)
                               (pi-resolve-deepseek-search-base-url config opt-url)
                             ) ;if
                   ) ;base-url
                  ) ;
              (unless
                (and (string? api-key) (not (string-null? (string-trim-both api-key))))
                (error 'error
                  "web_search: DeepSeek API key not found. Please set DEEPSEEK_API_KEY environment variable or configure api-key in auth.json"
                ) ;error
              ) ;unless
              (pi-execute-deepseek-web-search query urls api-key base-url proxy-alist timeout)
            ) ;let*
            ;; 默认 Google 后端：Gemini grounding API
            (let* ((api-key (pi-resolve-google-api-key config opt-key))
                   (base-url (pi-resolve-search-base-url config opt-url))
                   (opt-model (pi-keyword-value :model-id opts #f))
                   (ws-model (pi-web-search-config-model ws-config))
                   (model-id (pi-resolve-search-model-id config (or opt-model ws-model)))
                  ) ;
              (unless
                (and (string? api-key) (not (string-null? (string-trim-both api-key))))
                (error 'error
                  "web_search: Google API key not found. Please configure API key in auth.json or set GOOGLE_API_KEY environment variable"
                ) ;error
              ) ;unless
              (let* ((req-body (%build-search-request-body query urls))
                     (req-json-str (json->string req-body))
                     (endpoint (gemini-build-endpoint base-url model-id))
                     (headers (list (cons "Content-Type" "application/json") (cons "x-goog-api-key" api-key))
                     ) ;headers
                     (resp (%http-post-safe endpoint req-json-str headers proxy-alist timeout))
                     (status (or (%resp-field resp 'status-code) 0))
                     (body-str (or (%resp-field resp 'text) ""))
                    ) ;
                (%handle-response status body-str model-id)
              ) ;let*
            ) ;let*
          ) ;if
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
