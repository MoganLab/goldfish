;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes output)
  (import (scheme base)
    (scheme write)
    (liii string)
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

    (define (%block-alist blk)
      (if (pi-block? blk) (pi-block->alist blk) blk)
    ) ;define

    (define (%to-block-list blks)
      (cond ((vector? blks) (map %block-alist (vector->list blks)))
            ((list? blks) (map %block-alist blks))
            (else '())
      ) ;cond
    ) ;define

    (define (%has-text-block? blks)
      (let ((lst (%to-block-list blks)))
        (and (pair? lst)
          (any
            (lambda (blk)
              (let ((t (cdr (or (assoc 'type blk) (assoc "type" blk) '(#f . ""))))
                    (text (cdr (or (assoc 'text blk) (assoc "text" blk) '(#f
                                                                          . ""))))
                   ) ;
                (and (equal? t "text") (not (string-null? text)))
              ) ;let
            ) ;lambda
            lst
          ) ;any
        ) ;and
      ) ;let
    ) ;define

    (define (%print-text-blocks blks)
      (for-each
        (lambda (blk)
          (when
            (equal? (cdr (or (assoc 'type blk) (assoc "type" blk) '(#f . ""))) "text")
            (display (cdr (or (assoc 'text blk) (assoc "text" blk) '(#f . ""))))
            (newline)
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

    (define (pi-message-field msg key default)
      (let ((alist (cond ((pi-message? msg) (pi-message->alist msg))
                         ((pair? msg) msg)
                         (else #f)
                   ) ;cond
            ) ;alist
           ) ;
        (if alist (%assoc-field alist key default) default)
      ) ;let
    ) ;define

    (define (%block-text blk)
      (let ((b (if (pi-block? blk) (pi-block->alist blk) blk)))
        (if (and (pair? b) (equal? (%assoc-field b 'type "") "text"))
          (let ((text (%assoc-field b 'text "")))
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
        (let* ((msg (if (pi-message? last-msg) (pi-message->alist last-msg) last-msg))
               (stop-reason (cdr (or (assoc 'stopReason msg) (assoc "stopReason" msg) '(#f
                                                                                        . "stop")))
               ) ;stop-reason
               (content (cdr (or (assoc 'content msg) (assoc "content" msg) '(#f))))
               (err-msg (cdr (or (assoc 'errorMessage msg) (assoc "errorMessage" msg) '(#f
                                                                                        . #f)))
               ) ;err-msg
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
