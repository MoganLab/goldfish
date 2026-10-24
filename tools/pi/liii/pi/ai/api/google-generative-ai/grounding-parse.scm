;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai grounding-parse)
  (import (scheme base) (liii string))
  (export gemini-extract-grounding-details gemini-grounding-search-queries
    gemini-grounding-search-results gemini-grounding-citations
  ) ;export
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

    (define (%pair2 sym val)
      (list (cons sym val) (cons (symbol->string sym) val))
    ) ;define

    (define (%ensure-list x)
      (cond ((vector? x) (vector->list x))
            ((pair? x) (if (equal? x '(())) '() x))
            (else '())
      ) ;cond
    ) ;define

    (define (%make-search-result title url web)
      (append (%pair2 'title title)
        (%pair2 'url url)
        (%pair2 'source "google.groundingChunks")
        (%pair2 'type "web")
        (%pair2 'raw web)
      ) ;append
    ) ;define

    (define (%make-citation title url text start end idx support)
      (append (%pair2 'title title)
        (%pair2 'url url)
        (%pair2 'citedText text)
        (%pair2 'startIndex start)
        (%pair2 'endIndex end)
        (%pair2 'source "google.groundingSupports")
        (%pair2 'type "citation")
        (%pair2 'chunkIndex idx)
        (%pair2 'raw support)
      ) ;append
    ) ;define

    (define (%chunk-info chunk)
      (let ((web (%val "web" chunk #f)))
        (if
          (and (pair? web) (not (equal? web '(()))))
          (let* ((t (%val "title" web "Unknown"))
                 (title
                   (if (and (string? t) (not (string-null? t))) t "Unknown")
                 ) ;title
                 (u (%val "uri" web (%val "url" web "")))
                 (url (if (string? u) u ""))
                ) ;
            (cons (cons title url) web)
          ) ;let*
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (%find-grounding-metadata data)
      (cond
       ((not (or (pair? data) (vector? data))) #f)
       ((vector? data)
        (if (> (vector-length data) 0)
          (%find-grounding-metadata (vector-ref data 0))
          #f
        ) ;if
       ) ;
       ((equal? data '(())) #f)
       (else
         (let ((gm (%val "groundingMetadata" data #f)))
           (cond
            ((and (pair? gm) (not (equal? gm '(())))) gm)
            ((%val "candidates" data #f) => %find-grounding-metadata)
            ((or (%val "webSearchQueries" data #f)
               (%val "groundingChunks" data #f)
               (%val "groundingSupports" data #f)
             ) ;or
             data
            ) ;
            (else #f)
           ) ;cond
         ) ;let
       ) ;else
      ) ;cond
    ) ;define

    (define (%extract-search-queries gm)
      (let loop
        ((rem (%ensure-list (%val "webSearchQueries" gm '()))) (acc '()))
        (if (null? rem)
          (reverse acc)
          (let ((q (car rem)))
            (if
              (and (string? q) (not (string-null? (string-trim-both q))) (not (member q acc)))
              (loop (cdr rem) (cons q acc))
              (loop (cdr rem) acc)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%extract-search-results chunks-vec)
      (let loop
        ((i 0) (acc '()) (seen '()))
        (if (>= i (vector-length chunks-vec))
          (reverse acc)
          (let ((c-info (%chunk-info (vector-ref chunks-vec i))))
            (if c-info
              (let* ((title (car (car c-info)))
                     (url (cdr (car c-info)))
                     (web (cdr c-info))
                     (key (string-append url "\t" title "\tweb"))
                    ) ;
                (if (member key seen)
                  (loop (+ i 1) acc seen)
                  (loop (+ i 1) (cons (%make-search-result title url web) acc) (cons key seen))
                ) ;if
              ) ;let*
              (loop (+ i 1) acc seen)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%process-support-idx idx chunks-vec text start end support acc seen)
      (if (and (integer? idx) (>= idx 0) (< idx (vector-length chunks-vec)))
        (let ((c-info (%chunk-info (vector-ref chunks-vec idx))))
          (if c-info
            (let* ((title (car (car c-info)))
                   (url (cdr (car c-info)))
                   (key (string-append url "\t" title "\t" (or text "") "\tcitation"))
                  ) ;
              (if (member key seen)
                (values acc seen)
                (values (cons (%make-citation title url text start end idx support) acc)
                  (cons key seen)
                ) ;values
              ) ;if
            ) ;let*
            (values acc seen)
          ) ;if
        ) ;let
        (values acc seen)
      ) ;if
    ) ;define

    (define (%process-support support chunks-vec acc seen)
      (let* ((seg (%val "segment" support '()))
             (text (%val "text" seg #f))
             (start (%val "startIndex" seg #f))
             (end (%val "endIndex" seg #f))
             (indices (%ensure-list (%val "groundingChunkIndices" support '())))
            ) ;
        (let loop
          ((rem indices) (cur-acc acc) (cur-seen seen))
          (if (null? rem)
            (values cur-acc cur-seen)
            (let-values (((next-acc next-seen)
                          (%process-support-idx (car rem)
                            chunks-vec
                            text
                            start
                            end
                            support
                            cur-acc
                            cur-seen
                          ) ;%process-support-idx
                         ) ;
                        ) ;
              (loop (cdr rem) next-acc next-seen)
            ) ;let-values
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%extract-citations raw-supports chunks-vec)
      (let loop
        ((rem raw-supports) (acc '()) (seen '()))
        (if (null? rem)
          (reverse acc)
          (let-values (((next-acc next-seen) (%process-support (car rem) chunks-vec acc seen)))
            (loop (cdr rem) next-acc next-seen)
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    (define (%empty-grounding-details)
      (append (%pair2 'searchQueries '())
        (%pair2 'searchResults '())
        (%pair2 'citations '())
      ) ;append
    ) ;define

    (define (gemini-extract-grounding-details json-data)
      (let ((gm (%find-grounding-metadata json-data)))
        (if (not gm)
          (%empty-grounding-details)
          (let* ((queries (%extract-search-queries gm))
                 (chunks-vec (list->vector (%ensure-list (%val "groundingChunks" gm '()))))
                 (results (%extract-search-results chunks-vec))
                 (raw-supports (%ensure-list (%val "groundingSupports" gm '())))
                 (citations (%extract-citations raw-supports chunks-vec))
                ) ;
            (append (%pair2 'searchQueries queries)
              (%pair2 'searchResults results)
              (%pair2 'citations citations)
            ) ;append
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%grounding-details-dict? x)
      (and (pair? x) (or (assoc 'searchQueries x) (assoc "searchQueries" x)))
    ) ;define

    (define (%detail-field d key)
      (%val key
        (if (%grounding-details-dict? d) d (gemini-extract-grounding-details d))
        '()
      ) ;%val
    ) ;define

    (define (gemini-grounding-search-queries d)
      (%detail-field d 'searchQueries)
    ) ;define

    (define (gemini-grounding-search-results d)
      (%detail-field d 'searchResults)
    ) ;define

    (define (gemini-grounding-citations d)
      (%detail-field d 'citations)
    ) ;define

  ) ;begin
) ;define-library
