;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; run-agent-loop 的请求超时识别与重试调度
(define-library (liii pi agent loop-retry)
  (export pi-request-with-timeout-retry)
  (import (scheme base)
    (liii string)
    (liii time)
    (liii pi ai message)
    (liii pi ai retry)
  ) ;import
  (begin

    (define (%pi-extract-timeout-seconds err-msg fallback)
      (if (string? err-msg)
        (let ((pos (string-position "timed out after " err-msg)))
          (if pos
            (let* ((prefix-len (string-length "timed out after "))
                   (after (substring err-msg (+ pos prefix-len) (string-length err-msg)))
                   (s-pos (string-position "s" after))
                  ) ;
              (if s-pos
                (or (string->number (string-trim-both (substring after 0 s-pos))) fallback)
                fallback
              ) ;if
            ) ;let*
            fallback
          ) ;if
        ) ;let
        fallback
      ) ;if
    ) ;define

    (define (%pi-turn-timeout-error? stop-reason err-msg)
      (and (equal? stop-reason "error")
        (string? err-msg)
        (or (string-contains? err-msg "timed out after ")
          (string-contains? err-msg "Request timed out")
        ) ;or
      ) ;and
    ) ;define

    (define (%pi-resp-stop-reason resp)
      (cond ((pi-assistant-message? resp) (or (pi-assistant-message-stop-reason resp) ""))
            ((procedure? resp) (or (resp 'stop-reason) (resp 'stopReason) ""))
            ((pair? resp)
             (or (cdr (or (assoc 'stopReason resp) (assoc 'stop-reason resp) '(#f
                                                                               . #f))) "")
            ) ;
            (else "")
      ) ;cond
    ) ;define

    (define (%pi-resp-error-message resp)
      (cond ((pi-assistant-message? resp) (or (pi-assistant-message-error-message resp) ""))
            ((procedure? resp) (or (resp 'error-message) (resp 'errorMessage) ""))
            ((pair? resp)
             (or (cdr (or (assoc 'errorMessage resp) (assoc 'error-message resp) '(#f
                                                                                   . #f)))
               ""
             ) ;or
            ) ;
            (else "")
      ) ;cond
    ) ;define

    (define (%pi-assoc-val key alt-key alist default-val)
      (if (pair? alist)
        (let ((p (or (assoc key alist) (assoc alt-key alist))))
          (if p (cdr p) default-val)
        ) ;let
        default-val
      ) ;if
    ) ;define

    (define (%call-request-safely req-fn prov cfg msgs sp tools on-ev to)
      (catch 'http-client-timeout-error
        (lambda () (req-fn prov cfg msgs sp tools on-ev to))
        (lambda (tag . args)
          (pi-assistant-message '()
            :stop-reason "error"
            :error-message
            (string-append "Request timed out after " (number->string to) "s")
            :provider prov
          ) ;pi-assistant-message
        ) ;lambda
      ) ;catch
    ) ;define

    (define (pi-request-with-timeout-retry req-fn prov cfg msgs sp tools on-ev to emit)
      (let*-values (((settings) (%pi-assoc-val 'settings "settings" cfg '()))
                    ((opt-retry) (%pi-assoc-val 'retry "retry" cfg #f))
                    ((max-retries base-delay max-delay prov-retries)
                     (pi-resolve-retry-params settings opt-retry)
                    ) ;
                    ((eff-max-retries) (or prov-retries max-retries 3))
                   ) ;
        (let loop
          ((attempt 1))
          (let* ((resp-try (%call-request-safely req-fn prov cfg msgs sp tools on-ev to))
                 (stop-reason (%pi-resp-stop-reason resp-try))
                 (err-msg (%pi-resp-error-message resp-try))
                 (is-timeout? (%pi-turn-timeout-error? stop-reason err-msg))
                ) ;
            (if (and is-timeout? (<= attempt eff-max-retries))
              (let* ((to-sec (%pi-extract-timeout-seconds err-msg to))
                     (delay-sec (pi-calc-retry-delay attempt base-delay max-delay #f))
                     (retry-msg (string-append "[pi retry] Request timed out after "
                                  (number->string to-sec)
                                  "s, attempt "
                                  (number->string attempt)
                                  "/"
                                  (number->string eff-max-retries)
                                  ", waiting "
                                  (number->string delay-sec)
                                  "s"
                                ) ;string-append
                     ) ;retry-msg
                    ) ;
                (pi-ai-log! retry-msg)
                (when (procedure? emit)
                  (emit 'retry retry-msg)
                ) ;when
                (sleep delay-sec)
                (loop (+ attempt 1))
              ) ;let*
              resp-try
            ) ;if
          ) ;let*
        ) ;let
      ) ;let*-values
    ) ;define

  ) ;begin
) ;define-library
