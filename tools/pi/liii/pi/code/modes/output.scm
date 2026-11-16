;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes output)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii json)
    (liii list)
    (liii pi ai message)
  ) ;import
  (export display-stderr handle-print-result pi-last-message pi-message-field
    pi-last-assistant-text pi-content-text
  ) ;export
  (begin

    (define (display-stderr msg)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display msg p)
        (newline p)
      ) ;let
    ) ;define

    (define (%normalize-block blk)
      (if (or (pi-block? blk) (json-object? blk)) (or (json->pi-block blk) blk) blk)
    ) ;define

    (define (%to-block-list blks)
      (cond ((vector? blks) (map %normalize-block (vector->list blks)))
            ((list? blks) (map %normalize-block blks))
            (else '())
      ) ;cond
    ) ;define

    (define (%has-text-block? blks)
      (let ((lst (%to-block-list blks)))
        (and (pair? lst)
          (any
            (lambda (blk)
              (and (pi-text-block? blk)
                (let ((text (pi-text-block-text blk)))
                  (and (string? text) (not (string-null? text)))
                ) ;let
              ) ;and
            ) ;lambda
            lst
          ) ;any
        ) ;and
      ) ;let
    ) ;define

    (define (%print-text-blocks blks)
      (for-each
        (lambda (blk)
          (when (pi-text-block? blk)
            (let ((text (pi-text-block-text blk)))
              (when (string? text)
                (display text)
                (newline)
              ) ;when
            ) ;let
          ) ;when
        ) ;lambda
        (%to-block-list blks)
      ) ;for-each
    ) ;define

    (define (pi-last-message msgs)
      (and (pair? msgs) (car (reverse msgs)))
    ) ;define

    (define (%assoc-field alist key default)
      (let ((pair
              (or (assoc key alist)
                (assoc (if (symbol? key) (symbol->string key) (string->symbol key)) alist)
              ) ;or
            ) ;pair
           ) ;
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%message-record-field msg key default)
      (let ((k (if (symbol? key) (symbol->string key) key)))
        (cond ((string=? k "role") (pi-message-role msg))
              ((string=? k "content")
               (cond ((pi-user-message? msg) (pi-user-message-content msg))
                     ((pi-assistant-message? msg) (pi-assistant-message-content msg))
                     ((pi-tool-result-message? msg) (pi-tool-result-message-content msg))
                     (else default)
               ) ;cond
              ) ;
              ((and (pi-assistant-message? msg) (string=? k "stopReason"))
               (pi-assistant-message-stop-reason msg)
              ) ;
              ((and (pi-assistant-message? msg) (string=? k "errorMessage"))
               (pi-assistant-message-error-message msg)
              ) ;
              ((and (pi-assistant-message? msg) (string=? k "usage"))
               (pi-assistant-message-usage msg)
              ) ;
              ((and (pi-assistant-message? msg) (string=? k "provider"))
               (pi-assistant-message-provider msg)
              ) ;
              ((and (pi-assistant-message? msg) (string=? k "model"))
               (pi-assistant-message-model msg)
              ) ;
              ((and (pi-assistant-message? msg) (string=? k "api"))
               (pi-assistant-message-api msg)
              ) ;
              ((and (pi-tool-result-message? msg) (string=? k "toolCallId"))
               (pi-tool-result-message-tool-call-id msg)
              ) ;
              ((and (pi-tool-result-message? msg) (string=? k "toolName"))
               (pi-tool-result-message-tool-name msg)
              ) ;
              ((and (pi-tool-result-message? msg) (string=? k "isError"))
               (pi-tool-result-message-is-error msg)
              ) ;
              (else default)
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-message-field msg key default)
      (let ((m (if (or (pi-message? msg) (json-object? msg))
                 (or (json->pi-message msg) msg)
                 msg
               ) ;if
            ) ;m
           ) ;
        (cond ((pi-message? m) (%message-record-field m key default))
              ((pair? m) (%assoc-field m key default))
              (else default)
        ) ;cond
      ) ;let
    ) ;define

    (define (%block-text blk)
      (let ((b (%normalize-block blk)))
        (if (pi-text-block? b)
          (let ((text (pi-text-block-text b)))
            (if (string? text) text "")
          ) ;let
          ""
        ) ;if
      ) ;let
    ) ;define

    (define (pi-content-text content)
      (apply string-append (map %block-text (%to-block-list content)))
    ) ;define

    (define (%message-text msg)
      (pi-content-text (pi-message-field msg 'content '()))
    ) ;define

    (define (pi-last-assistant-text msgs)
      (or
        (let ((msg
                (find
                  (lambda (msg) (not (string-null? (string-trim-both (%message-text msg)))))
                  (reverse msgs)
                ) ;find
              ) ;msg
             ) ;
          (and msg (%message-text msg))
        ) ;let
        ""
      ) ;or
    ) ;define

    (define (handle-print-result last-msg)
      (if (not (or (pair? last-msg) (pi-message? last-msg)))
        (begin
          (display-stderr "Warning: No assistant response received")
          0
        ) ;begin
        (let* ((msg (if (or (pi-message? last-msg) (json-object? last-msg))
                      (or (json->pi-message last-msg) last-msg)
                      last-msg
                    ) ;if
               ) ;msg
               (raw-stop-reason (pi-message-field msg 'stopReason "stop"))
               (stop-reason (if (string? raw-stop-reason) raw-stop-reason "stop"))
               (content (pi-message-field msg 'content '()))
               (err-msg (pi-message-field msg 'errorMessage #f))
              ) ;
          (cond
           ((or (string=? stop-reason "error") (string=? stop-reason "aborted"))
            (display-stderr (or err-msg (string-append "Request " stop-reason)))
            1
           ) ;
           ((string=? stop-reason "length")
            (%print-text-blocks content)
            (display-stderr "Response was truncated: the model reached its output token limit."
            ) ;display-stderr
            1
           ) ;
           ((%has-text-block? content) (%print-text-blocks content) 0)
           (else (display-stderr "Warning: Model returned no text output") 0)
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
