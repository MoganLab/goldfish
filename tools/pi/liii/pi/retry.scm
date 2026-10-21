;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi retry)
  (import (scheme base)
    (scheme char)
    (scheme write)
    (liii string)
    (liii hash-table)
    (liii date-time)
    (liii pi time)
    (liii pi config types)
    (liii pi event)
  ) ;import
  (export pi-is-retriable-error? pi-calc-retry-delay pi-resolve-retry-params
    gemini-is-retriable-error? anthropic-is-retriable-error?
    pi-extract-retry-after pi-parse-retry-after pi-log-retry
    set-pi-retry-logger!
  ) ;export
  (begin

    (define (pi-is-retriable-error? status-code response-body-str)
      (or (= status-code 408)
        (= status-code 409)
        (= status-code 429)
        (>= status-code 500)
        (= status-code 0)
      ) ;or
    ) ;define

    (define gemini-is-retriable-error? pi-is-retriable-error?)
    (define anthropic-is-retriable-error? pi-is-retriable-error?)

    (define (%safe-ref table key)
      (let ((alt-k (if (symbol? key) (symbol->string key) (string->symbol key))))
        (cond ((hash-table? table)
               (or (hash-table-ref table key) (hash-table-ref table alt-k))
              ) ;
              ((procedure? table)
               (catch #t (lambda () (or (table key #f) (table alt-k #f))) (lambda _ #f))
              ) ;
              ((pair? table)
               (let ((entry (or (assoc key table) (assoc alt-k table))))
                 (and (pair? entry) (cdr entry))
               ) ;let
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-extract-retry-after resp)
      (let ((headers (%safe-ref resp 'headers)))
        (if (not headers)
          #f
          (let* ((raw (or (%safe-ref headers "retry-after") (%safe-ref headers 'retry-after)))
                 (val (if (string? raw) (string-trim-both raw) raw))
                ) ;
            (cond
             ((and (string? val) (not (string-null? val))) val)
             ((and (number? val) (>= val 0)) val)
             (else #f)
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%month->number m)
      (let ((p (assoc (string-downcase m)
                 '(("jan" . 1)
                   ("feb" . 2)
                   ("mar" . 3)
                   ("apr" . 4)
                   ("may" . 5)
                   ("jun" . 6)
                   ("jul" . 7)
                   ("aug" . 8)
                   ("sep" . 9)
                   ("oct" . 10)
                   ("nov" . 11)
                   ("dec" . 12))
               ) ;assoc
            ) ;p
           ) ;
        (and p (cdr p))
      ) ;let
    ) ;define

    (define (%clean-tokens raw)
      (let loop
        ((ts raw) (acc '()))
        (if (null? ts)
          (reverse acc)
          (let ((t (string-trim-both (car ts))))
            (loop (cdr ts) (if (string-null? t) acc (cons t acc)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%parse-http-time parts)
      (and (= (length parts) 3)
        (let ((h (string->number (car parts)))
              (m (string->number (cadr parts)))
              (s (string->number (caddr parts)))
             ) ;
          (and (integer? h) (integer? m) (integer? s) (list h m s))
        ) ;let
      ) ;and
    ) ;define

    (define (%parse-http-date str now-sec)
      (catch #t
        (lambda ()
          (let* ((tokens (%clean-tokens (string-split str " ")))
                 (body
                   (if
                     (and (pair? tokens)
                       (or (string-contains (car tokens) ",")
                         (member (string-downcase (car tokens))
                           '("mon" "tue" "wed" "thu" "fri" "sat" "sun")
                         ) ;member
                       ) ;or
                     ) ;and
                     (cdr tokens)
                     tokens
                   ) ;if
                 ) ;body
                ) ;
            (and (>= (length body) 4)
              (let* ((day (string->number (car body)))
                     (mon (%month->number (cadr body)))
                     (year (string->number (caddr body)))
                     (hms (%parse-http-time (string-split (list-ref body 3) ":")))
                    ) ;
                (and (integer? day)
                  mon
                  (integer? year)
                  hms
                  (let* ((ts (make-timestamp year mon day (car hms) (cadr hms) (caddr hms) (utc-timezone))
                         ) ;ts
                         (diff (- (timestamp->utc-posix-time ts) now-sec))
                        ) ;
                    (max 0.0 (* 1.0 diff))
                  ) ;let*
                ) ;and
              ) ;let*
            ) ;and
          ) ;let*
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define

    (define* (pi-parse-retry-after val (current-time-sec #f))
      (cond ((not val) #f)
            ((number? val) (if (>= val 0) (* 1.0 val) #f))
            ((string? val)
             (let* ((trimmed (string-trim-both val)) (num (string->number trimmed)))
               (cond ((and (real? num) (>= num 0)) (* 1.0 num))
                     ((and (real? num) (< num 0)) #f)
                     (else
                       (let ((now (or current-time-sec (pi-current-time-seconds))))
                         (%parse-http-date trimmed now)
                       ) ;let
                     ) ;else
               ) ;cond
             ) ;let*
            ) ;
            (else #f)
      ) ;cond
    ) ;define*

    (define* (pi-calc-retry-delay attempt base-delay-ms max-agent-delay-ms (retry-after #f))
      (let* ((raw-delay-sec
               (if (and (number? retry-after) (>= retry-after 0))
                 (* 1.0 retry-after)
                 (/
                   (* base-delay-ms (expt 2 (max 0 (- attempt 1))))
                   1000.0
                 ) ;/
               ) ;if
             ) ;raw-delay-sec
             (min-sec 0.1)
             (max-sec (/ max-agent-delay-ms 1000.0))
            ) ;
        (min (max raw-delay-sec min-sec) (max min-sec max-sec))
      ) ;let*
    ) ;define*

    (define *pi-retry-logger* #f)

    (define (set-pi-retry-logger! logger)
      (set! *pi-retry-logger* logger)
    ) ;define

    (define* (pi-log-retry status attempt max-retries delay-sec (retry-after #f) (port #f))
      (let* ((ra-part (if (and (number? retry-after) (>= retry-after 0))
                        (string-append " (retry-after: " (number->string retry-after) "s)")
                        ""
                      ) ;if
             ) ;ra-part
             (msg (string-append "[pi retry] HTTP "
                    (number->string status)
                    ", attempt "
                    (number->string attempt)
                    "/"
                    (number->string max-retries)
                    ", waiting "
                    (number->string delay-sec)
                    "s"
                    ra-part
                  ) ;string-append
             ) ;msg
            ) ;
        (if (procedure? *pi-retry-logger*)
          (*pi-retry-logger* msg)
          (let ((p (or port (if (defined? '*stderr*) *stderr* (current-error-port)))))
            (display (string-append msg "\n") p)
            (flush-output-port p)
          ) ;let
        ) ;if
      ) ;let*
    ) ;define*

    (define (%safe-assoc key alist)
      (and (or (pair? alist) (pi-settings? alist) (pi-retry-config? alist))
        (or (assoc key alist)
          (let ((str-k (if (symbol? key) (symbol->string key) (string->symbol key))))
            (assoc str-k alist)
          ) ;let
        ) ;or
      ) ;and
    ) ;define

    (define (pi-resolve-retry-params settings opt-retry)
      (let* ((retry-cfg
               (or opt-retry (cdr (or (%safe-assoc 'retry settings) '(#f))))
             ) ;retry-cfg
             (enabled? (cdr (or (%safe-assoc 'enabled retry-cfg) '(#f . #t))))
             (max-retries
               (if enabled? (cdr (or (%safe-assoc 'max-retries retry-cfg) '(#f
                                                                            . 3))) 0)
             ) ;max-retries
             (base-delay (cdr (or (%safe-assoc 'base-delay-ms retry-cfg) '(#f
                                                                           . 2000))))
             (max-delay (cdr (or (%safe-assoc 'max-agent-delay-ms retry-cfg) '(#f
                                                                               . 60000))))
             (provider-raw (cdr (or (%safe-assoc 'provider-max-retries retry-cfg) '(#f
                                                                                    . #f)))
             ) ;provider-raw
             (provider-nested
               (let ((section (cdr (or (%safe-assoc 'provider retry-cfg) '(#f
                                                                           . #f)))))
                 (and (pair? section) (cdr (or (%safe-assoc 'max-retries section) '(#f
                                                                                    . #f))))
               ) ;let
             ) ;provider-nested
             (provider-max-retries
               (if enabled?
                 (cond ((number? provider-raw) provider-raw)
                       ((number? provider-nested) provider-nested)
                       (else max-retries)
                 ) ;cond
                 0
               ) ;if
             ) ;provider-max-retries
            ) ;
        (values max-retries base-delay max-delay provider-max-retries)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
