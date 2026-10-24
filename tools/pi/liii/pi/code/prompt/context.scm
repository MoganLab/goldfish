;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code prompt context)
  (import (scheme base) (scheme file) (liii string) (liii path) (liii os))
  (export pi-resolve-prompt-input pi-load-project-context)
  (begin

    (define (pi-resolve-prompt-input input)
      (if (and (string? input) (file-exists? input))
        (catch #t
          (lambda ()
            (let ((content (path-read-text (path input))))
              (if (string? content) content input)
            ) ;let
          ) ;lambda
          (lambda (k . args) input)
        ) ;catch
        input
      ) ;if
    ) ;define

    (define (%find-first-context-file dir candidates)
      (let loop-dir
        ((cur-dir dir))
        (let loop-cand
          ((cands candidates))
          (if (null? cands)
            (let* ((p (path cur-dir)) (parent (path-parent p)))
              (if (or (not parent) (equal? parent p) (string-null? (path->string parent)))
                #f
                (loop-dir (path->string parent))
              ) ;if
            ) ;let*
            (let* ((filename (car cands))
                   (filepath (path->string (path-join (path cur-dir) (path filename))))
                  ) ;
              (if (path-file? (path filepath))
                (let ((content
                        (catch #t (lambda () (path-read-text (path filepath))) (lambda (k . args) #f))
                      ) ;content
                     ) ;
                  (if (string? content)
                    (list (cons 'path filepath) (cons 'content content))
                    (loop-cand (cdr cands))
                  ) ;if
                ) ;let
                (loop-cand (cdr cands))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (pi-load-project-context dir)
      (let ((candidates '("AGENTS.override.md" "AGENTS.md" "AGENTS.MD"
                          "CLAUDE.md" "CLAUDE.MD")
            ) ;candidates
           ) ;
        (%find-first-context-file dir candidates)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
