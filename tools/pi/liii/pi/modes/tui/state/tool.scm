;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui state tool)
  (import (scheme base)
    (liii error)
    (liii pi modes tui state types)
    (liii pi modes tui tool-card)
  ) ;import
  (export state-active-tool-card state-handle-tool-start!
    state-tick-active-tools! state-handle-tool-end!
  ) ;export
  (begin

    (define (state-active-tool-card st)
      (%validate-state st "state-active-tool-card")
      (let loop
        ((rest (reverse (state-messages st))))
        (cond ((null? rest) #f)
              ((and (pi-tui-tool-card? (car rest))
                 (eq? (tool-card-status (car rest)) 'running)
               ) ;and
               (car rest)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (state-handle-tool-start! st id tool-name args)
      (%validate-state st "state-handle-tool-start!")
      (unless (string? id)
        (type-error "state-handle-tool-start!: expected string for id")
      ) ;unless
      (unless (string? tool-name)
        (type-error "state-handle-tool-start!: expected string for tool-name")
      ) ;unless
      (let* ((summary (format-tool-args-summary tool-name args))
             (card (make-pi-tui-tool-card :id id :tool-name tool-name :args-summary summary))
            ) ;
        (%state-set-messages-raw! st (append (state-messages st) (list card)))
        (%state-set-dirty-raw! st #t)
        card
      ) ;let*
    ) ;define

    (define (state-tick-active-tools! st)
      (%validate-state st "state-tick-active-tools!")
      (let loop
        ((rest (state-messages st)) (ticked? #f))
        (if (null? rest)
          (begin
            (when ticked?
              (%state-set-dirty-raw! st #t)
            ) ;when
            ticked?
          ) ;begin
          (let ((item (car rest)))
            (if (and (pi-tui-tool-card? item) (eq? (tool-card-status item) 'running))
              (begin
                (tool-card-tick! item)
                (loop (cdr rest) #t)
              ) ;begin
              (loop (cdr rest) ticked?)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (state-handle-tool-end! st id raw-res is-err? . maybe-elapsed)
      (%validate-state st "state-handle-tool-end!")
      (unless (string? id)
        (type-error "state-handle-tool-end!: expected string for id")
      ) ;unless
      (let* ((elapsed (if (pair? maybe-elapsed) (car maybe-elapsed) 0.0))
             (res-summary (format-tool-result-summary raw-res is-err?))
             (target-card
               (let loop
                 ((rest (reverse (state-messages st))))
                 (cond ((null? rest) #f)
                       ((and (pi-tui-tool-card? (car rest)) (equal? (tool-card-id (car rest)) id))
                        (car rest)
                       ) ;
                       (else (loop (cdr rest)))
                 ) ;cond
               ) ;let
             ) ;target-card
            ) ;
        (when target-card
          (if is-err?
            (tool-card-fail! target-card elapsed res-summary)
            (tool-card-complete! target-card elapsed res-summary)
          ) ;if
          (%state-set-dirty-raw! st #t)
        ) ;when
        target-card
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
