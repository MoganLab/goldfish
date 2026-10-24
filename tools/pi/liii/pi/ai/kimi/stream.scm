;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai kimi stream)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii json)
    (liii http)
    (liii time)
    (liii pi ai time)
    (liii pi ai event)
    (liii pi ai retry)
    (liii pi ai message)
    (liii pi ai kimi core)
    (liii pi ai kimi msg)
    (liii pi ai kimi request)
    (liii pi ai kimi stream-parse)
    (liii pi ai multikey)
  ) ;import
  (export kimi-request-stream)
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (%build-stream-cb stream-state on-event effective-idle on-idle err-acc)
      (let* ((last-act (pi-current-monotonic-seconds))
             (decoder (make-kimi-stream-decoder stream-state on-event))
            ) ;
        (lambda (chunk)
          (let ((now (pi-current-monotonic-seconds)))
            (vector-set! err-acc 0 (string-append (vector-ref err-acc 0) chunk))
            (if (and effective-idle (> (- now last-act) effective-idle))
              (begin
                (on-idle)
                #f
              ) ;begin
              (begin
                (set! last-act now)
                (decoder chunk)
                #t
              ) ;begin
            ) ;if
          ) ;let
        ) ;lambda
      ) ;let*
    ) ;define

    (define (%finalize-stream stream-state)
      (finish-kimi-stream stream-state #f)
      (let* ((blocks (vector-ref stream-state 0))
             (stop-reason (vector-ref stream-state 3))
             (err-msg (vector-ref stream-state 4))
            ) ;
        (pi-assistant-message blocks
          :stop-reason   stop-reason
          :usage         (vector-ref stream-state 2)
          :error-message err-msg
        ) ;pi-assistant-message
      ) ;let*
    ) ;define

    (define (%make-idle-timeout-error eff-idle)
      (pi-assistant-message '()
        :stop-reason "error"
        :usage       (kimi-map-usage '())
        :error-message
        (string-append "Kimi stream idle timeout after "
          (number->string eff-idle)
          " seconds"
        ) ;string-append
      ) ;pi-assistant-message
    ) ;define

    (define (%extract-kimi-err-msg body status)
      (let ((parsed-msg (extract-kimi-stream-error-message body)))
        (if (and (string? parsed-msg) (not (string-null? parsed-msg)))
          (string-append "HTTP " (number->string status) ": " parsed-msg)
          (string-append "HTTP " (number->string status) " stream error")
        ) ;if
      ) ;let
    ) ;define

    (define (%make-stream-error status resp err-acc)
      (let* ((resp-err (or (%resp-field resp 'reason) (%resp-field resp 'error) ""))
             (err-text (if (string? resp-err) (string-trim resp-err) ""))
             (acc-body (vector-ref err-acc 0))
             (err-msg
               (cond
                ((and (zero? status) (not (string-null? err-text)))
                 (string-append "Request failed: " err-text)
                ) ;
                ((not (string-null? acc-body)) (%extract-kimi-err-msg acc-body status))
                (else (string-append "HTTP " (number->string status) " stream error"))
               ) ;cond
             ) ;err-msg
            ) ;
        (pi-assistant-message '()
          :stop-reason   "error"
          :usage         (kimi-map-usage '())
          :error-message err-msg
        ) ;pi-assistant-message
      ) ;let*
    ) ;define

    (define (%post-stream url req-json-str headers proxy-alist cb timeout eff-idle)
      (catch #t
        (lambda ()
          (http-post url
            :data         req-json-str
            :headers      headers
            :proxy        proxy-alist
            :stream       #t
            :callback     cb
            :timeout      timeout
            :idle-timeout eff-idle
          ) ;http-post
        ) ;lambda
        (lambda (tag . args)
          (let ((err-msg (if (pair? args) (car args) "Network error")))
            (list (cons 'status-code 0) (cons 'reason err-msg) (cons 'text ""))
          ) ;let
        ) ;lambda
      ) ;catch
    ) ;define

    (define (%init-kimi-stream-state)
      (vector '() #f (kimi-map-usage '()) "stop" #f '())
    ) ;define

    (define (%run-stream-loop config url req-json-str proxy-alist on-ev timeout
              opt-retry eff-idle
            ) ;%run-stream-loop
      (let* ((settings (cdr (or (assoc 'settings config) '(#f))))
             (pool (pi-multikey-pool-of config))
            ) ;
        (let-values (((max-r base-delay max-delay provider-r)
                      (pi-resolve-retry-params settings opt-retry)
                     ) ;
                    ) ;
          (let loop
            ((attempt 1) (api-key (pi-multikey-request-key config)))
            (let* ((headers (kimi-build-headers api-key))
                   (stream-state (%init-kimi-stream-state))
                   (idle-timed-out? #f)
                   (emitted? #f)
                   (wrapped-on-ev (if (procedure? on-ev) (lambda (ev) (set! emitted? #t) (on-ev ev)) #f)
                   ) ;wrapped-on-ev
                   (err-acc (vector ""))
                   (cb (%build-stream-cb stream-state
                         wrapped-on-ev
                         eff-idle
                         (lambda () (set! idle-timed-out? #t))
                         err-acc
                       ) ;%build-stream-cb
                   ) ;cb
                   (resp (%post-stream url req-json-str headers proxy-alist cb timeout eff-idle))
                   (status (or (%resp-field resp 'status-code) 0))
                  ) ;
              (cond (idle-timed-out? (%make-idle-timeout-error eff-idle))
                    ((and pool (not emitted?) (or (= status 429) (= status 403)))
                     (let ((next (pi-multikey-rotate! config api-key status)))
                       (if (eq? next 'exhausted)
                         (%make-stream-error status resp err-acc)
                         (loop 1 next)
                       ) ;if
                     ) ;let
                    ) ;
                    ((kimi-is-retriable-error? status "")
                     (if (<= attempt provider-r)
                       (let* ((ra-raw (pi-extract-retry-after resp))
                              (ra-sec (pi-parse-retry-after ra-raw))
                              (delay-sec (pi-calc-retry-delay attempt base-delay max-delay ra-sec))
                             ) ;
                         (pi-log-retry status attempt provider-r delay-sec ra-sec)
                         (sleep delay-sec)
                         (loop (+ attempt 1) api-key)
                       ) ;let*
                       (%make-stream-error status resp err-acc)
                     ) ;if
                    ) ;
                    ((or (zero? status) (not (= status 200)))
                     (%make-stream-error status resp err-acc)
                    ) ;
                    (else (%finalize-stream stream-state))
              ) ;cond
            ) ;let*
          ) ;let
        ) ;let-values
      ) ;let*
    ) ;define

    (define* (kimi-request-stream config
               messages
               (system-instruction #f)
               (tool-names #f)
               (on-event #f)
               (timeout 300)
               (opt-retry #f)
               (idle-timeout #f)
             ) ;kimi-request-stream
      (let* ((model-id (cdr (assoc 'model-id config)))
             (model-meta (cdr (assoc 'model-meta config)))
             (proxy-url (cdr (assoc 'proxy-url config)))
             (base-url (cdr (or (assoc 'base-url model-meta) '(#f
                                                               . "https://api.kimi.com/coding/v1")))
             ) ;base-url
             (max-tokens (cdr (or (assoc 'max-tokens model-meta) '(#f . 8192))))
             (url (kimi-build-endpoint base-url))
             (eff-tool-names (or tool-names (cdr (assoc 'tools config))))
             (tool-resolver (if (assoc 'tool-resolver config)
                              (cdr (assoc 'tool-resolver config))
                              (lambda (name) #f)
                            ) ;if
             ) ;tool-resolver
             (tool-decls (kimi-build-tool-declarations-from-names eff-tool-names tool-resolver)
             ) ;tool-decls
             (kimi-msgs (kimi-build-messages messages system-instruction))
             (req-body (kimi-build-request-body model-id kimi-msgs #t tool-decls max-tokens))
             (req-json-str (json->string req-body))
             (proxy-alist (kimi-build-proxy-alist proxy-url))
             (eff-idle (or idle-timeout timeout))
            ) ;
        (%run-stream-loop config url req-json-str proxy-alist on-event timeout
          opt-retry eff-idle
        ) ;%run-stream-loop
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
