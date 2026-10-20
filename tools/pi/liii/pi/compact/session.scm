;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compact session)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii json)
    (liii uuid)
    (liii pi time)
    (liii pi event)
    (liii pi session)
    (liii pi message)
    (liii pi compact tokens)
    (liii pi compact cut)
    (liii pi compact fileops)
    (liii pi compact prompt)
    (liii pi compact summary)
  ) ;import
  (export pi-read-session-entries pi-project-session-messages
    pi-last-compaction-summary pi-append-compaction-entry! pi-compact-session!
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

    (define (%parse-line line)
      (let ((trimmed (string-trim line)))
        (if (string-null? trimmed)
          #f
          (catch #t (lambda () (string->json trimmed)) (lambda _ #f))
        ) ;if
      ) ;let
    ) ;define

    (define (pi-read-session-entries file-path)
      (let ((p-str (if (string? file-path) file-path (path->string file-path))))
        (if (not (file-exists? p-str))
          '()
          (let loop
            ((lines (path-read-lines p-str)) (acc '()))
            (if (null? lines)
              (reverse acc)
              (let ((entry (%parse-line (car lines))))
                (loop (cdr lines) (if entry (cons entry acc) acc))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%entry-index-by-id entries id)
      (let loop
        ((rest entries) (idx 0))
        (cond ((null? rest) #f)
              ((equal? (%ref (car rest) "id") id) idx)
              (else (loop (cdr rest) (+ idx 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%last-compaction-index entries)
      (let loop
        ((rest entries) (idx 0) (found #f))
        (if (null? rest)
          found
          (loop (cdr rest)
            (+ idx 1)
            (if (equal? (%ref (car rest) "type") "compaction") idx found)
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    (define (%as-pi-message msg)
      (let* ((msg (pi-normalize-session-msg msg))
             (role (%ref msg "role"))
             (content (%ref msg "content"))
            ) ;
        (cond ((equal? role "user") (pi-user-message content))
              ((equal? role "assistant")
               (pi-assistant-message content
                 (or (%ref msg "stopReason") "stop")
                 (%ref msg "errorMessage")
                 (%ref msg "usage")
               ) ;pi-assistant-message
              ) ;
              ((equal? role "toolResult")
               (pi-tool-result-message (or (%ref msg "toolCallId") "")
                 (or (%ref msg "toolName") "")
                 content
                 (eq? #t (%ref msg "isError"))
               ) ;pi-tool-result-message
              ) ;
              (else msg)
        ) ;cond
      ) ;let*
    ) ;define

    (define (%messages-from entries start)
      (let loop
        ((rest entries) (idx 0) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let ((msg
                  (and (equal? (%ref (car rest) "type") "message") (%ref (car rest) "message"))
                ) ;msg
               ) ;
            (loop (cdr rest)
              (+ idx 1)
              (if (and msg (>= idx start)) (cons (%as-pi-message msg) acc) acc)
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-last-compaction-summary entries)
      (let ((idx (%last-compaction-index entries)))
        (and idx (%ref (list-ref entries idx) "summary"))
      ) ;let
    ) ;define

    (define (pi-project-session-messages entries)
      (let* ((cidx (%last-compaction-index entries)))
        (if (not cidx)
          (%messages-from entries 0)
          (let* ((compaction (list-ref entries cidx))
                 (summary (%ref compaction "summary"))
                 (kept-id (%ref compaction "firstKeptEntryId"))
                 (start (%entry-index-by-id entries kept-id))
                 (start (if start start (+ cidx 1)))
                ) ;
            (append (list (pi-compaction-summary-message summary))
              (%messages-from entries start)
            ) ;append
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%last-entry-id entries)
      (if (null? entries) #f (%ref (car (reverse entries)) "id"))
    ) ;define

    (define (pi-append-compaction-entry! file-path summary first-kept-entry-id
              tokens-before details
            ) ;pi-append-compaction-entry!
      (let* ((entries (pi-read-session-entries file-path))
             (parent-id (%last-entry-id entries))
             (id (uuid4))
             (entry (list (cons "type" "compaction")
                      (cons "id" id)
                      (cons "parentId" parent-id)
                      (cons "timestamp" (pi-iso-8601-utc-now))
                      (cons "summary" summary)
                      (cons "firstKeptEntryId" first-kept-entry-id)
                      (cons "tokensBefore" tokens-before)
                      (cons "details" details)
                    ) ;list
             ) ;entry
            ) ;
        (append-session-entry! file-path (pi-serialize-event entry))
        id
      ) ;let*
    ) ;define

    (define (%kept-message-count entries start)
      (let loop
        ((rest entries) (idx 0) (acc 0))
        (if (null? rest)
          acc
          (loop (cdr rest)
            (+ idx 1)
            (if
              (and (>= idx start) (equal? (%ref (car rest) "type") "message"))
              (+ acc 1)
              acc
            ) ;if
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    (define* (pi-compact-session! config file-path settings (timeout 600) (reason "manual"))
      (let* ((entries (pi-read-session-entries file-path))
             (all-msgs (%messages-from entries 0))
             (tokens-before (cdr (assoc 'tokens (pi-context-token-estimate all-msgs))))
             (keep
               (let ((p (assoc 'keep-recent-tokens settings)))
                 (if p (cdr p) 20000)
               ) ;let
             ) ;keep
             (cut (pi-find-cut-point entries keep))
             (cut-index (cdr (assoc 'first-kept-index cut)))
             (summarize-msgs (pi-messages-to-summarize entries cut-index))
            ) ;
        (when (null? summarize-msgs)
          (error 'compact-error
            "Session is too short to compact: no messages would be summarized"
          ) ;error
        ) ;when
        (let* ((entry (list-ref entries cut-index))
               (first-kept-id (%ref entry "id"))
               (previous-summary (pi-last-compaction-summary entries))
               (gen (pi-generate-summary config summarize-msgs previous-summary timeout))
               (summary (cdr (assoc 'summary gen)))
               (ops (pi-extract-file-ops summarize-msgs))
               (lists (pi-compute-file-lists ops))
               (details
                 (list (cons "reason" reason)
                   (cons "readFiles" (cdr (assoc 'read-files lists)))
                   (cons "modifiedFiles" (cdr (assoc 'modified-files lists)))
                 ) ;list
               ) ;details
               (id (pi-append-compaction-entry! file-path summary first-kept-id
                     tokens-before details
                   ) ;pi-append-compaction-entry!
               ) ;id
              ) ;
          (list (cons 'id id)
            (cons 'summary summary)
            (cons 'first-kept-entry-id first-kept-id)
            (cons 'tokens-before tokens-before)
            (cons 'summarized-count (length summarize-msgs))
            (cons 'kept-count (%kept-message-count entries cut-index))
            (cons 'usage (cdr (assoc 'usage gen)))
          ) ;list
        ) ;let*
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
