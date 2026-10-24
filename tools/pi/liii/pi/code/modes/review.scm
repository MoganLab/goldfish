;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes review)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii list)
    (liii pi code args)
    (liii pi agent tools)
    (liii pi ai message)
    (liii pi code modes output)
    (liii pi code modes flow)
    (liii pi code review)
  ) ;import
  (export run-code-review-flow)
  (begin

    (define %review-system-prompt
      "You are a code reviewer operating inside pi. You have no tools: do not call, request, or simulate tool calls. All file contents to review are provided in the user message. Return only the JSON findings object requested there."
    ) ;define

    (define (%review-parsed parsed)
      (if (pi-args-ref parsed 'system-prompt)
        parsed
        (cons (cons 'system-prompt %review-system-prompt)
          (filter
            (lambda (pair) (not (and (pair? pair) (eq? (car pair) 'system-prompt))))
            parsed
          ) ;filter
        ) ;cons
      ) ;if
    ) ;define

    (define (%review-config cfg)
      (cons (cons 'tools '())
        (filter
          (lambda (pair) (not (eq? (car pair) 'tools)))
          cfg
        ) ;filter
      ) ;cons
    ) ;define

    (define (%normalize-blocks content)
      (let ((items (cond ((vector? content) (vector->list content))
                         ((list? content) content)
                         (else '())
                   ) ;cond
            ) ;items
           ) ;
        (map (lambda (blk) (if (pi-block? blk) (pi-block->alist blk) blk)) items)
      ) ;let
    ) ;define

    (define (%message-alist msg)
      (cond ((pi-message? msg) (pi-message->alist msg))
            ((pair? msg) msg)
            (else #f)
      ) ;cond
    ) ;define

    (define (%message-field key alt-key msg default)
      (let ((alist (%message-alist msg)))
        (if alist
          (let ((pair (or (assoc key alist) (assoc alt-key alist))))
            (if pair (cdr pair) default)
          ) ;let
          default
        ) ;if
      ) ;let
    ) ;define

    (define (%message-text msg)
      (let ((alist (%message-alist msg)))
        (if (not alist)
          ""
          (let* ((content (cdr (or (assoc "content" alist) (assoc 'content alist) '(#f))))
                 (normalized (cons (cons "content" (%normalize-blocks content)) alist))
                ) ;
            (extract-assistant-text normalized)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%last-assistant-text msgs)
      (let loop
        ((rest (reverse msgs)))
        (cond ((null? rest) "")
              (else
                (let ((text (%message-text (car rest))))
                  (if (string-null? (string-trim-both text)) (loop (cdr rest)) text)
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%print-review-result res-msgs)
      (let* ((last-msg (and (pair? res-msgs) (car (reverse res-msgs))))
             (stop-reason (%message-field "stopReason" 'stopReason last-msg #f))
             (err-msg (%message-field "errorMessage" 'errorMessage last-msg #f))
            ) ;
        (cond
         ((or (equal? stop-reason "error") (equal? stop-reason "aborted"))
          (display-stderr (or err-msg (string-append "Request " stop-reason)))
          1
         ) ;
         ((equal? stop-reason "length")
          (display-stderr "Response was truncated: the model reached its output token limit."
          ) ;display-stderr
          1
         ) ;
         (else
           (let ((findings (pi-parse-review-findings (%last-assistant-text res-msgs))))
             (display (pi-format-review-findings findings))
             (newline)
             0
           ) ;let
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (run-code-review-flow parsed cfg mode)
      (let* ((file-args (pi-args-ref parsed 'messages))
             (explicit (and (pair? file-args) file-args))
             (collected (pi-collect-changed-files
                          :ref     (or (pi-args-ref parsed 'review-ref) "HEAD")
                          :staged  (pi-args-ref parsed 'staged)
                          :files   explicit
                          :timeout (pi-resolve-timeout parsed cfg)
                        ) ;pi-collect-changed-files
             ) ;collected
             (paths (cdr (assoc 'paths collected)))
             (notice (cdr (assoc 'message collected)))
            ) ;
        (if (null? paths)
          (let ((msg "No changed files found. Specify file paths or make some changes first."))
            (if (eq? mode 'json) (display-stderr msg) (begin (display msg) (newline)))
            0
          ) ;let
          (begin
            (when notice
              (display-stderr notice)
            ) ;when
            (let* ((records (pi-review-file-records paths))
                   (prompt (pi-build-review-prompt records))
                   (rcfg (%review-config cfg))
                   (review-parsed (%review-parsed parsed))
                  ) ;
              (if (eq? mode 'json)
                (run-session-flow review-parsed rcfg prompt emit-json-event (lambda _ 0))
                (run-session-flow review-parsed rcfg prompt #f %print-review-result)
              ) ;if
            ) ;let*
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
