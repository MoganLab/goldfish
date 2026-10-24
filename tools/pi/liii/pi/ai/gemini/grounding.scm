;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai gemini grounding)
  (import (scheme base) (liii list) (liii pi ai gemini grounding-parse))
  (export gemini-build-search-tools gemini-inject-google-search
    gemini-extract-grounding-details gemini-grounding-search-queries
    gemini-grounding-search-results gemini-grounding-citations
  ) ;export
  (begin

    (define (%ensure-list x)
      (cond ((vector? x) (vector->list x))
            ((pair? x) (if (equal? x '(())) '() x))
            (else '())
      ) ;cond
    ) ;define

    (define (%has-google-search? tool)
      (and (pair? tool)
        (or (assoc "googleSearch" tool)
          (assoc "google_search" tool)
          (assoc 'googleSearch tool)
          (assoc 'google_search tool)
        ) ;or
      ) ;and
    ) ;define

    (define (%tools-contain-google-search? tools)
      (any %has-google-search? (%ensure-list tools))
    ) ;define

    (define (%google-search-tool-spec)
      (list (cons "googleSearch" '(())))
    ) ;define

    (define (%is-tool-object? x)
      (and (pair? x)
        (pair? (car x))
        (or (assoc "functionDeclarations" x)
          (assoc 'functionDeclarations x)
          (assoc "googleSearch" x)
          (assoc 'googleSearch x)
          (assoc "google_search" x)
          (assoc 'google_search x)
        ) ;or
      ) ;and
    ) ;define

    (define (%tools-list? x)
      (and (pair? x) (%is-tool-object? (car x)))
    ) ;define

    (define (%append-search-tool cur-tools)
      (if (%tools-contain-google-search? cur-tools)
        cur-tools
        (if (vector? cur-tools)
          (vector-append cur-tools (vector (%google-search-tool-spec)))
          (append cur-tools (list (%google-search-tool-spec)))
        ) ;if
      ) ;if
    ) ;define

    (define (%inject-into-req req)
      (let ((entry (or (assoc "tools" req) (assoc 'tools req))))
        (if entry
          (let ((new-tools (%append-search-tool (cdr entry))))
            (map
              (lambda (p)
                (if (or (equal? (car p) "tools") (equal? (car p) 'tools))
                  (cons (car p) new-tools)
                  p
                ) ;if
              ) ;lambda
              req
            ) ;map
          ) ;let
          (cons (cons "tools" (vector (%google-search-tool-spec))) req)
        ) ;if
      ) ;let
    ) ;define

    (define (gemini-inject-google-search target)
      (cond ((vector? target) (%append-search-tool target))
            ((%tools-list? target) (%append-search-tool target))
            ((or (null? target) (equal? target '(())))
             (list (cons "tools" (vector (%google-search-tool-spec))))
            ) ;
            (else (%inject-into-req target))
      ) ;cond
    ) ;define

    (define* (gemini-build-search-tools (tool-decls '()))
      (cond ((or (null? tool-decls) (not tool-decls)) (vector (%google-search-tool-spec)))
            ((or (vector? tool-decls) (%tools-list? tool-decls))
             (gemini-inject-google-search tool-decls)
            ) ;
            ((pair? tool-decls)
             (vector (list (cons "functionDeclarations" (list->vector tool-decls)))
               (%google-search-tool-spec)
             ) ;vector
            ) ;
            (else (vector (%google-search-tool-spec)))
      ) ;cond
    ) ;define*

  ) ;begin
) ;define-library
