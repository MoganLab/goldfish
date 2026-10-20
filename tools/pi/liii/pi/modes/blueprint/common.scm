;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes blueprint common)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi args)
    (liii pi tools)
    (liii pi modes output)
    (liii pi modes flow)
    (liii pi blueprint store)
  ) ;import
  (export pi-bp-mode-extend-tools pi-bp-mode-config pi-bp-mode-state-path
    pi-bp-mode-out pi-bp-mode-emit pi-bp-mode-timeout pi-bp-mode-error-text
    pi-bp-mode-session-failure
  ) ;export
  (begin

    (define %blueprint-tools
      '("blueprint_update" "blueprint_status" "blueprint_next")
    ) ;define

    (define (%replace-field key value cfg)
      (cons (cons key value) (filter (lambda (pair) (not (eq? (car pair) key))) cfg))
    ) ;define

    (define (pi-bp-mode-extend-tools base parsed)
      (if (pi-args-ref parsed 'no-tools)
        base
        (let* ((extras (exclude-tools-from-list %blueprint-tools (pi-args-ref parsed 'exclude-tools))
               ) ;extras
               (missing
                 (filter (lambda (name) (not (member name base))) extras)
               ) ;missing
              ) ;
          (append base missing)
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-bp-mode-config parsed cfg)
      (%replace-field 'tools
        (pi-bp-mode-extend-tools (cdr (assoc 'tools cfg)) parsed)
        cfg
      ) ;%replace-field
    ) ;define

    (define (pi-bp-mode-state-path parsed)
      (let* ((cwd (path->string (path-cwd))) (raw (pi-args-ref parsed 'blueprint-file)))
        (if
          (and (string? raw) (not (string-null? (string-trim-both raw))))
          (pi-blueprint-resolve-path (string-trim-both raw) cwd)
          (pi-blueprint-default-state-path cwd)
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-bp-mode-out mode text)
      (if (eq? mode 'json) (display-stderr text) (begin (display text) (newline)))
    ) ;define

    (define (pi-bp-mode-emit mode)
      (if (eq? mode 'json) emit-json-event #f)
    ) ;define

    (define (pi-bp-mode-timeout parsed cfg)
      (or (pi-args-ref parsed 'timeout) (cdr (or (assoc 'timeout cfg) '(#f
                                                                        . 120))))
    ) ;define

    (define (pi-bp-mode-error-text tag info)
      (cond
       ((and (pair? info) (string? (car info))) (car info))
       ((pair? info) (object->string (car info)))
       (else (object->string tag))
      ) ;cond
    ) ;define

    (define (pi-bp-mode-session-failure res-msgs)
      (let* ((msg (pi-last-message res-msgs))
             (stop (pi-message-field msg 'stopReason #f))
             (err (pi-message-field msg 'errorMessage #f))
            ) ;
        (cond ((or (equal? stop "error") (equal? stop "aborted"))
               (or err (string-append "Request " stop))
              ) ;
              ((equal? stop "length")
               "Response was truncated: the model reached its output token limit."
              ) ;
              (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
