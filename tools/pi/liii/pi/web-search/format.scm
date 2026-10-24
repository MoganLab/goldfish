;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search format)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi code tools truncate)
    (liii pi code tools read)
    (liii pi ai gemini grounding)
  ) ;import
  (export pi-format-web-search-result)
  (begin

    (define (%val key alist default)
      (if
        (and (pair? alist) (not (equal? alist '(()))))
        (let ((p
                (or (assoc key alist)
                  (if (string? key) (assoc (string->symbol key) alist) #f)
                  (if (symbol? key) (assoc (symbol->string key) alist) #f)
                ) ;or
              ) ;p
             ) ;
          (if p (cdr p) default)
        ) ;let
        default
      ) ;if
    ) ;define

    (define (%ensure-list x)
      (cond ((vector? x) (vector->list x))
            ((pair? x) (if (equal? x '(())) '() x))
            (else '())
      ) ;cond
    ) ;define

    (define (%source-item? x)
      (and (pair? x) (or (assoc 'url x) (assoc "url" x)))
    ) ;define

    (define (%extract-unique-sources citations)
      (let loop
        ((rem (%ensure-list citations)) (acc '()) (seen '()))
        (if (null? rem)
          (reverse acc)
          (let* ((item (car rem))
                 (url (%val 'url item (%val "url" item "")))
                 (title (%val 'title item (%val "title" item "Unknown")))
                ) ;
            (if (or (not (string? url)) (string-null? (string-trim-both url)) (member url seen))
              (loop (cdr rem) acc seen)
              (let ((src (list (cons 'title title) (cons 'url url))))
                (loop (cdr rem) (cons src acc) (cons url seen))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%extract-additional-results search-results sources)
      (let* ((source-urls (map (lambda (s) (%val 'url s "")) sources)))
        (let loop
          ((rem (%ensure-list search-results)) (acc '()) (seen '()))
          (if (null? rem)
            (reverse acc)
            (let* ((item (car rem))
                   (url (%val 'url item (%val "url" item "")))
                   (title (%val 'title item (%val "title" item "Unknown")))
                   (key (string-append url "\t" title))
                  ) ;
              (cond
               ((or (not (string? url)) (string-null? (string-trim-both url)))
                (loop (cdr rem) acc seen)
               ) ;
               ((member url source-urls) (loop (cdr rem) acc seen))
               ((member key seen) (loop (cdr rem) acc seen))
               (else
                 (let ((entry (list (cons 'title title) (cons 'url url))))
                   (loop (cdr rem) (cons entry acc) (cons key seen))
                 ) ;let
               ) ;else
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%format-source-line i src)
      (let* ((title (%val 'title src ""))
             (url (%val 'url src ""))
             (has-title?
               (and (string? title)
                 (not (string-null? (string-trim-both title)))
                 (not (string=? title "Unknown"))
               ) ;and
             ) ;has-title?
            ) ;
        (if has-title?
          (string-append "[" (number->string i) "] " title " - " url)
          (string-append "[" (number->string i) "] " url)
        ) ;if
      ) ;let*
    ) ;define

    (define (%format-additional-line i res)
      (let* ((title (%val 'title res ""))
             (url (%val 'url res ""))
             (has-title?
               (and (string? title)
                 (not (string-null? (string-trim-both title)))
                 (not (string=? title "Unknown"))
               ) ;and
             ) ;has-title?
            ) ;
        (if has-title?
          (string-append (number->string i) ". " title " - " url)
          (string-append (number->string i) ". " url)
        ) ;if
      ) ;let*
    ) ;define

    (define (%append-sources summary sources)
      (if (or (null? sources) (string-contains? summary "## Sources"))
        summary
        (let* ((lines (map %format-source-line (iota (length sources) 1) sources))
               (sec (string-append "## Sources\n" (string-join lines "\n")))
              ) ;
          (if (string-null? (string-trim-both summary))
            sec
            (string-append summary "\n\n" sec)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (%append-additional summary additional)
      (if (or (null? additional)
            (string-contains? summary "## Additional Search Results")
          ) ;or
        summary
        (let* ((lines (map %format-additional-line (iota (length additional) 1) additional))
               (sec (string-append "## Additional Search Results\n" (string-join lines "\n")))
              ) ;
          (if (string-null? (string-trim-both summary))
            sec
            (string-append summary "\n\n" sec)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-format-web-search-result text grounding-data . opts)
      (let* ((clean-text (if (string? text) text ""))
             (model-id (pi-keyword-value :model-id opts "gemini-2.5-flash"))
             (max-lines (pi-keyword-value :max-lines opts PI_DEFAULT_MAX_LINES))
             (max-bytes (pi-keyword-value :max-bytes opts PI_DEFAULT_MAX_BYTES))
             (opt-sources (pi-keyword-value :sources opts #f))
             (queries (gemini-grounding-search-queries grounding-data))
             (results (gemini-grounding-search-results grounding-data))
             (citations (gemini-grounding-citations grounding-data))
             (sources
               (cond ((pair? opt-sources) opt-sources)
                     ((not (null? citations)) (%extract-unique-sources citations))
                     (else '())
               ) ;cond
             ) ;sources
             (additional (%extract-additional-results results sources))
             (summary1 (%append-sources clean-text sources))
             (summary2 (%append-additional summary1 additional))
             (trunc-res (pi-truncate-head summary2 max-lines max-bytes))
             (is-trunc? (pi-trunc-result-truncated? trunc-res))
             (final-text (if is-trunc?
                           (string-append (pi-trunc-result-content trunc-res) "\n\n[Truncated]")
                           (pi-trunc-result-content trunc-res)
                         ) ;if
             ) ;final-text
             (details
               (list (cons 'sources sources)
                 (cons 'searchQueries queries)
                 (cons 'searchResults results)
                 (cons 'citations citations)
                 (cons 'model model-id)
                 (cons 'grounded (or (not (null? sources)) (not (null? results))))
                 (cons 'resultCount (if (not (null? results)) (length results) (length sources)))
               ) ;list
             ) ;details
            ) ;
        (list
          (cons 'content (vector (list (cons 'type "text") (cons 'text final-text))))
          (cons 'text final-text)
          (cons 'sources sources)
          (cons 'details details)
          (cons 'isError #f)
        ) ;list
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
