;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui)
  (import (scheme base) (liii error) (liii os) (liii time) (liii tui terminal))
  (export with-pi-tui-terminal run-tui-loop run-tui-flow)
  (begin

    (define (with-pi-tui-terminal term thunk)
      (unless (terminal? term)
        (type-error "with-pi-tui-terminal: expected terminal")
      ) ;unless
      (unless (procedure? thunk)
        (type-error "with-pi-tui-terminal: expected procedure for thunk")
      ) ;unless
      (dynamic-wind
        (lambda ()
          (terminal-write term "\x1b;[?1049h")
          (terminal-hide-cursor! term)
          (catch #t (lambda () (console-raw-mode! #t)) (lambda _ #f))
          (terminal-clear term)
          (terminal-flush term)
        ) ;lambda
        thunk
        (lambda ()
          (terminal-show-cursor! term)
          (terminal-write term "\x1b;[?1049l")
          (catch #t (lambda () (console-raw-mode! #f)) (lambda _ #f))
          (terminal-flush term)
        ) ;lambda
      ) ;dynamic-wind
    ) ;define

    (define (%parse-loop-opts opts)
      (let loop
        ((rest opts) (max-ticks #f) (on-key #f) (interval 0.03))
        (cond ((null? rest) (values max-ticks on-key interval))
              ((eq? (car rest) :max-ticks) (loop (cddr rest) (cadr rest) on-key interval))
              ((eq? (car rest) :on-key) (loop (cddr rest) max-ticks (cadr rest) interval))
              ((eq? (car rest) :interval) (loop (cddr rest) max-ticks on-key (cadr rest)))
              ((exact-integer? (car rest)) (loop (cdr rest) (car rest) on-key interval))
              (else (loop (cdr rest) max-ticks on-key interval))
        ) ;cond
      ) ;let
    ) ;define

    (define (run-tui-loop term . opts)
      (unless (terminal? term)
        (type-error "run-tui-loop: expected terminal")
      ) ;unless
      (let-values (((max-ticks on-key tick-interval) (%parse-loop-opts opts)))
        (let loop
          ((ticks 0))
          (if (and max-ticks (>= ticks max-ticks))
            0
            (let ((key (terminal-poll-key term)))
              (when (and key on-key)
                (on-key key)
              ) ;when
              (cond ((or (eq? key 'escape) (equal? key "q") (equal? key "Q")) 0)
                    (else (when (standard-terminal? term) (sleep tick-interval)) (loop (+ ticks 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let-values
    ) ;define

    (define (run-tui-flow parsed cfg mode initial-msg . maybe-term)
      (let ((term
              (if (and (pair? maybe-term) (terminal? (car maybe-term)))
                (car maybe-term)
                (make-standard-terminal)
              ) ;if
            ) ;term
           ) ;
        (with-pi-tui-terminal term (lambda () (run-tui-loop term)))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
