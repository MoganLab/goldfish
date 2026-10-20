;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi gemini stream-result)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii pi message)
    (liii pi gemini core)
    (liii pi gemini stream-parse)
    (liii pi gemini thinking)
  ) ;import
  (export gemini-finalize-stream-success gemini-classify-stream-result)
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (gemini-finalize-stream-success stream-state on-event)
      (gemini-stream-stop-active-block! stream-state on-event)
      (let* ((blocks (vector-ref stream-state 0))
             (has-fn? (pair? (vector-ref stream-state 3)))
             (raw-finish (vector-ref stream-state 4))
             (finish-reason (or raw-finish "STOP"))
             (stop-reason (gemini-map-finish-reason finish-reason has-fn?))
             (err-msg (if (equal? stop-reason "error") finish-reason #f))
            ) ;
        (pi-assistant-message blocks
          :stop-reason   stop-reason
          :error-message err-msg
          :usage         (vector-ref stream-state 2)
        ) ;pi-assistant-message
      ) ;let*
    ) ;define

    (define (gemini-classify-stream-result stream-state idle-timed-out? resp
              status eff-idle on-event error-body-acc
            ) ;gemini-classify-stream-result
      (let ((resp-err (or (%resp-field resp 'error) (%resp-field resp 'reason) "")))
        (cond
         ((or idle-timed-out?
            (string-contains? (string-downcase resp-err) "too slow")
            (string-contains? (string-downcase resp-err) "low speed")
            (string-contains? (string-downcase resp-err) "timed out")
          ) ;or
          (pi-assistant-message (vector-ref stream-state 0)
            :stop-reason "error"
            :error-message
            (string-append "Stream idle timed out after "
              (number->string eff-idle)
              " seconds"
            ) ;string-append
            :usage (vector-ref stream-state 2)
          ) ;pi-assistant-message
         ) ;
         ((and (not (zero? status)) (not (= status 200)))
          (let ((body
                  (if (not (string-null? (vector-ref error-body-acc 0)))
                    (vector-ref error-body-acc 0)
                    (or (%resp-field resp 'body) (%resp-field resp 'text) "")
                  ) ;if
                ) ;body
               ) ;
            (pi-assistant-message (vector-ref stream-state 0)
              :stop-reason "error"
              :error-message
              (let ((parsed-msg (extract-stream-error-message body)))
                (if (and parsed-msg (not (string=? parsed-msg "")))
                  (string-append "HTTP " (number->string status) ": " parsed-msg)
                  (string-append "HTTP " (number->string status) " error")
                ) ;if
              ) ;let
              :usage (vector-ref stream-state 2)
            ) ;pi-assistant-message
          ) ;let
         ) ;
         ((zero? status)
          (pi-assistant-message (vector-ref stream-state 0)
            :stop-reason   "error"
            :error-message (format-failed-stream-msg status resp)
            :usage         (vector-ref stream-state 2)
          ) ;pi-assistant-message
         ) ;
         (else (gemini-finalize-stream-success stream-state on-event))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
