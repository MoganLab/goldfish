;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compact summary)
  (import (scheme base)
    (liii string)
    (liii pi message)
    (liii pi agent)
    (liii pi compact serialize)
    (liii pi compact prompt)
  ) ;import
  (export pi-summarization-failure pi-generate-summary)
  (begin

    (define (%ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    (define (%message-alist msg)
      (if (pi-message? msg) (pi-message->alist msg) msg)
    ) ;define

    (define (pi-summarization-failure response label)
      (let* ((msg (%message-alist response)) (stop (%ref msg "stopReason")))
        (cond
         ((equal? stop "error")
          (string-append label " failed: " (or (%ref msg "errorMessage") "Unknown error"))
         ) ;
         ((equal? stop "length")
          (string-append label
            " failed: generation hit the token cap and the summary is incomplete"
          ) ;string-append
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    (define (%has-tool-call? content)
      (let loop
        ((rest (cond ((vector? content) (vector->list content))
                     ((pair? content) content)
                     (else '())
               ) ;cond
         ) ;rest
        ) ;
        (cond ((null? rest) #f)
              ((equal?
                 (%ref (if (pi-block? (car rest)) (pi-block->alist (car rest)) (car rest))
                   "type"
                 ) ;%ref
                 "toolCall"
               ) ;equal?
               #t
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define* (pi-generate-summary config msgs previous-summary (timeout 600))
      (let* ((provider (cdr (or (assoc 'provider config) '(#f . "google"))))
             (conversation (pi-serialize-conversation msgs))
             (prompt (pi-build-summarization-prompt conversation previous-summary))
             (user-msg (pi-user-message prompt))
             (resp (pi-request-provider-stream provider
                     config
                     (list user-msg)
                     PI_SUMMARIZATION_SYSTEM_PROMPT
                     '()
                     #f
                     timeout
                   ) ;pi-request-provider-stream
             ) ;resp
             (failure (pi-summarization-failure resp "Summarization"))
            ) ;
        (when failure
          (error 'summarization-error failure)
        ) ;when
        (when (%has-tool-call? (%ref (%message-alist resp) "content"))
          (error 'summarization-error "Summarization attempted to call a tool")
        ) ;when
        (let ((text (extract-assistant-text resp)))
          (when (string-null? text)
            (error 'summarization-error "Summarization returned an empty summary")
          ) ;when
          (list (cons 'summary text) (cons 'usage (pi-assistant-message-usage resp)))
        ) ;let
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
