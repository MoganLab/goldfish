;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compact cut)
  (import (scheme base) (liii list) (liii pi compact tokens))
  (export pi-entry-message pi-entry-id pi-valid-cut-point? pi-find-cut-point
    pi-messages-to-summarize
  ) ;export
  (begin

    (define (%ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    (define (pi-entry-id entry)
      (%ref entry "id")
    ) ;define

    (define (pi-entry-message entry)
      (and (equal? (%ref entry "type") "message") (%ref entry "message"))
    ) ;define

    (define (pi-valid-cut-point? entry)
      (let ((msg (pi-entry-message entry)))
        (and (pair? msg)
          (let ((role (%ref msg "role")))
            (or (equal? role "user") (equal? role "assistant"))
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (%token-count entry)
      (let ((msg (pi-entry-message entry)))
        (if msg (pi-estimate-message-tokens msg) 0)
      ) ;let
    ) ;define

    (define (%last lst)
      (if (null? lst) #f (car (reverse lst)))
    ) ;define

    (define (%all-indices start count)
      (iota (- count start) start)
    ) ;define

    (define (%cut-points entries start count)
      (filter (lambda (idx) (pi-valid-cut-point? (list-ref entries idx)))
        (%all-indices start count)
      ) ;filter
    ) ;define

    (define (%pick-cut cut-points i)
      (let ((candidates (filter (lambda (c) (>= c i)) cut-points)))
        (if (pair? candidates) (car candidates) (%last cut-points))
      ) ;let
    ) ;define

    (define (%scan-back entries cut-points start count keep)
      (let loop
        ((i (- count 1)) (acc 0))
        (cond ((< i start) (car cut-points))
              ((= (%token-count (list-ref entries i)) 0) (loop (- i 1) acc))
              ((>= (+ acc (%token-count (list-ref entries i))) keep) (%pick-cut cut-points i))
              (else
                (loop (- i 1) (+ acc (%token-count (list-ref entries i))))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-find-cut-point entries keep-recent-tokens . opt-start)
      (let* ((start
               (if (and (pair? opt-start) (number? (car opt-start))) (car opt-start) 0)
             ) ;start
             (count (length entries))
             (cut-points (%cut-points entries start count))
            ) ;
        (list (cons 'first-kept-index
                (if (null? cut-points)
                  start
                  (%scan-back entries cut-points start count keep-recent-tokens)
                ) ;if
              ) ;cons
          (cons 'is-split-turn #f)
        ) ;list
      ) ;let*
    ) ;define

    (define (pi-messages-to-summarize entries first-kept-index)
      (filter (lambda (m) m)
        (map pi-entry-message (list-take entries first-kept-index))
      ) ;filter
    ) ;define

  ) ;begin
) ;define-library
