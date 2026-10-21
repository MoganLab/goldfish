;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui runner)
  (import (scheme base)
    (liii error)
    (liii os)
    (liii tui terminal)
    (liii tui main-screen)
    (liii pi modes tui loop)
    (liii pi modes tui editor)
    (liii pi modes tui state)
    (liii pi modes tui layout)
  ) ;import
  (export with-pi-tui-terminal run-tui-flow)
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

    (define (run-tui-flow parsed cfg mode initial-msg . maybe-term)
      (let* ((term
               (if (and (pair? maybe-term) (terminal? (car maybe-term)))
                 (car maybe-term)
                 (make-standard-terminal)
               ) ;if
             ) ;term
             (screen (make-main-screen term))
             (ed (make-pi-tui-editor :focused? #t))
            ) ;
        (let-values (((cols rows) (detect-terminal-dimensions term)))
          (let* ((st (make-pi-tui-state :input-editor ed :cols cols :rows rows :dirty? #t)))
            (with-pi-tui-terminal term
              (lambda () (run-tui-loop term :state st :screen screen))
            ) ;with-pi-tui-terminal
          ) ;let*
        ) ;let-values
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
