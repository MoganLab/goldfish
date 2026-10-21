;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search execute)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii http)
    (liii error)
    (liii pi read)
    (liii pi gemini grounding)
    (liii pi web-search schema)
    (liii pi web-search config)
    (liii pi web-search format)
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
            (let loop
              ((i 0) (acc '()))
              (if (>= i (vector-length parts))
                (string-join (reverse acc) "")
                (let* ((p (vector-ref parts i)) (t (cdr (or (assoc "text" p) '(#f
                                                                               . #f)))))
                  (if (string? t) (loop (+ i 1) (cons t acc)) (loop (+ i 1) acc))
                ) ;let*
              ) ;if
            ) ;let
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

    (define (pi-execute-web-search args . opts)
      (let* ((timed-out? (pi-keyword-value :timed-out? opts #f)))
        (when timed-out?
          (error 'error "Command timed out")
        ) ;when
        (pi-validate-web-search-args args)
        (let* ((config (pi-keyword-value :config opts '()))
               (opt-key (pi-keyword-value :api-key opts #f))
               (api-key (pi-resolve-google-api-key config opt-key))
               (opt-url (pi-keyword-value :base-url opts #f))
               (base-url (pi-resolve-search-base-url config opt-url))
               (opt-proxy (pi-keyword-value :proxy-url opts #f))
               (proxy-url (pi-resolve-search-proxy-url config opt-proxy))
               (opt-model (pi-keyword-value :model-id opts #f))
               (model-id (pi-resolve-search-model-id config opt-model))
               (timeout (pi-keyword-value :timeout opts 600))
               (query (pi-arg-ref args "query"))
               (urls (pi-arg-ref args "urls" '()))
              ) ;
          (unless (pi-valid-key? api-key)
            (error 'error
              "web_search: Google API key not found. Please configure API key in auth.json or set GOOGLE_API_KEY environment variable"
            ) ;error
          ) ;unless
          (let* ((req-body (%build-search-request-body query urls))
                 (req-json-str (json->string req-body))
                 (endpoint (string-append base-url "/models/" model-id ":generateContent"))
                 (headers (list (cons "Content-Type" "application/json") (cons "x-goog-api-key" api-key))
                 ) ;headers
                 (proxy-alist (pi-resolve-search-proxy-alist proxy-url))
                 (resp (%http-post-safe endpoint req-json-str headers proxy-alist timeout))
                 (status (or (%resp-field resp 'status-code) 0))
                 (body-str (or (%resp-field resp 'text) ""))
                ) ;
            (%handle-response status body-str model-id)
          ) ;let*
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
