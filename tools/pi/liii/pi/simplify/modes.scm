;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi simplify modes)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii list)
    (liii pi code args)
    (liii pi agent tools)
    (liii pi ai message)
    (liii pi code modes output)
    (liii pi code modes flow)
    (liii pi simplify ranges)
    (liii pi simplify prompt)
    (liii pi simplify parse)
    (liii pi simplify format)
  ) ;import
  (export run-simplify-flow)
  (begin

    (define %simplify-system-prompt
      "You are a code simplifier operating inside pi. You have no tools: do not call, request, or simulate tool calls. All relevant code excerpts are provided in the user message. Return only the JSON suggestions object requested there."
    ) ;define

    (define (%simplify-parsed parsed)
      (if (pi-args-ref parsed 'system-prompt)
        parsed
        (cons (cons 'system-prompt %simplify-system-prompt)
          (filter
            (lambda (pair) (not (and (pair? pair) (eq? (car pair) 'system-prompt))))
            parsed
          ) ;filter
        ) ;cons
      ) ;if
    ) ;define

    (define (%simplify-config cfg)
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

    (define (%last-message msgs)
      (and (pair? msgs) (car (reverse msgs)))
    ) ;define

    (define (%request-failure res-msgs)
      (let* ((last-msg (%last-message res-msgs))
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
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    (define (%print-simplify-result res-msgs)
      (let ((failure (%request-failure res-msgs)))
        (if failure
          failure
          (let ((suggestions (pi-parse-simplify-suggestions (%last-assistant-text res-msgs))))
            (display (pi-format-simplify-suggestions suggestions))
            (newline)
            0
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%print-apply-result res-msgs)
      (let ((failure (%request-failure res-msgs)))
        (if failure failure (handle-print-result (%last-message res-msgs)))
      ) ;let
    ) ;define

    (define (%collect parsed cfg)
      (let ((file-args (pi-args-ref parsed 'messages)))
        (pi-collect-simplify-files
          :ref     (or (pi-args-ref parsed 'review-ref) "HEAD")
          :staged  (pi-args-ref parsed 'staged)
          :files   (and (pair? file-args) file-args)
          :timeout (pi-resolve-timeout parsed cfg)
        ) ;pi-collect-simplify-files
      ) ;let
    ) ;define

    (define (%run-read-only parsed cfg mode files)
      (let* ((prompt (pi-build-simplify-prompt files)) (rcfg (%simplify-config cfg)))
        (if (eq? mode 'json)
          (run-session-flow (%simplify-parsed parsed)
            rcfg
            prompt
            emit-json-event
            (lambda _ 0)
          ) ;run-session-flow
          (run-session-flow (%simplify-parsed parsed)
            rcfg
            prompt
            #f
            %print-simplify-result
          ) ;run-session-flow
        ) ;if
      ) ;let*
    ) ;define

    (define (%run-apply parsed cfg mode files)
      (let ((prompt (pi-build-simplify-apply-prompt files)))
        (if (eq? mode 'json)
          (run-session-flow parsed cfg prompt emit-json-event (lambda _ 0))
          (run-session-flow parsed cfg prompt #f %print-apply-result)
        ) ;if
      ) ;let
    ) ;define

    (define (run-simplify-flow parsed cfg mode)
      (let* ((collected (%collect parsed cfg))
             (paths (cdr (assoc 'paths collected)))
             (files (cdr (assoc 'files collected)))
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
            (if (pi-args-ref parsed 'apply)
              (%run-apply parsed cfg mode files)
              (%run-read-only parsed cfg mode files)
            ) ;if
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
