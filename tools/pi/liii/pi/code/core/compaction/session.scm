;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core compaction session)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii uuid)
    (liii pi ai time)
    (liii pi code core session-manager)
    (liii pi code core session-entry)
    (liii pi code core compaction tokens)
    (liii pi code core compaction cut)
    (liii pi code core compaction fileops)
    (liii pi code core compaction prompt)
    (liii pi code core compaction summary)
  ) ;import
  (export pi-read-session-entries pi-last-compaction-summary
    pi-append-compaction-entry! pi-compact-session! make-pi-compaction-result
    pi-compaction-result? pi-compaction-result-id pi-compaction-result-summary
    pi-compaction-result-first-kept-entry-id pi-compaction-result-tokens-before
    pi-compaction-result-summarized-count pi-compaction-result-kept-count
    pi-compaction-result-usage
  ) ;export
  (begin

    (define-record-type <pi-compaction-result>
      (make-pi-compaction-result id summary first-kept-entry-id tokens-before
        summarized-count kept-count usage
      ) ;make-pi-compaction-result
      pi-compaction-result?
      (id pi-compaction-result-id)
      (summary pi-compaction-result-summary)
      (first-kept-entry-id pi-compaction-result-first-kept-entry-id)
      (tokens-before pi-compaction-result-tokens-before)
      (summarized-count pi-compaction-result-summarized-count)
      (kept-count pi-compaction-result-kept-count)
      (usage pi-compaction-result-usage)
    ) ;define-record-type

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
          (filter (lambda (e) e) (map %parse-line (path-read-lines p-str)))
        ) ;if
      ) ;let
    ) ;define

    (define (%last-compaction-index entries)
      (let ((rev-idx
              (list-index
                (lambda (e)
                  (and (or (pi-session-entry? e) (json-object? e))
                    (equal? (pi-session-entry-type (or (json->pi-session-entry e) e)) "compaction")
                  ) ;and
                ) ;lambda
                (reverse entries)
              ) ;list-index
            ) ;rev-idx
           ) ;
        (and rev-idx (- (length entries) 1 rev-idx))
      ) ;let
    ) ;define

    ;; 条目到上下文消息的映射统一收敛到 session-manager 的单一实现（1201），
    ;; 压缩层不再维护第二份映射副本
    (define (%messages-from entries start)
      (apply append (map pi-entry-to-context-messages (list-tail entries start)))
    ) ;define

    (define (pi-last-compaction-summary entries)
      (let ((idx (%last-compaction-index entries)))
        (and idx
          (let* ((raw (list-ref entries idx)) (rec (or (json->pi-session-entry raw) raw)))
            (if (pi-session-compaction-entry? rec)
              (pi-session-compaction-entry-summary rec)
              (%ref raw "summary")
            ) ;if
          ) ;let*
        ) ;and
      ) ;let
    ) ;define

    (define (%last-entry-id entries)
      (and (pair? entries)
        (let* ((raw (car (reverse entries))) (rec (or (json->pi-session-entry raw) raw)))
          (if (pi-session-entry? rec) (pi-session-entry-id rec) (%ref raw "id"))
        ) ;let*
      ) ;and
    ) ;define

    (define (pi-append-compaction-entry! file-path summary first-kept-entry-id
              tokens-before details
            ) ;pi-append-compaction-entry!
      (let* ((entries (pi-read-session-entries file-path))
             (parent-id (%last-entry-id entries))
             (id (uuid4))
             (entry (pi-session-compaction-entry id
                      parent-id
                      (pi-iso-8601-utc-now)
                      summary
                      first-kept-entry-id
                      tokens-before
                      details
                    ) ;pi-session-compaction-entry
             ) ;entry
            ) ;
        (append-session-entry! file-path (json->string (pi-session-entry->json entry)))
        id
      ) ;let*
    ) ;define

    (define (%kept-message-count entries start)
      (count
        (lambda (entry idx)
          (and (>= idx start)
            (equal? (pi-session-entry-type (or (json->pi-session-entry entry) entry))
              "message"
            ) ;equal?
          ) ;and
        ) ;lambda
        entries
        (iota (length entries))
      ) ;count
    ) ;define

    (define* (pi-compact-session! config
               file-path
               settings
               (timeout 600)
               (reason "manual")
               (request-fn #f)
             ) ;pi-compact-session!
      (let* ((entries (pi-read-session-entries file-path))
             (all-msgs (%messages-from entries 0))
             (tokens-before (pi-context-token-estimate-tokens (pi-context-token-estimate all-msgs))
             ) ;tokens-before
             (keep (pi-compaction-settings-keep-recent-tokens settings))
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
               (gen (pi-generate-summary config summarize-msgs previous-summary timeout request-fn)
               ) ;gen
               (summary (cdr (assoc 'summary gen)))
               (ops (pi-extract-file-ops summarize-msgs))
               (lists (pi-compute-file-lists ops))
               (details (list (cons "reason" reason)
                          (cons "readFiles" (pi-file-lists-read-files lists))
                          (cons "modifiedFiles" (pi-file-lists-modified-files lists))
                        ) ;list
               ) ;details
               (id (pi-append-compaction-entry! file-path summary first-kept-id
                     tokens-before details
                   ) ;pi-append-compaction-entry!
               ) ;id
              ) ;
          (make-pi-compaction-result id
            summary
            first-kept-id
            tokens-before
            (length summarize-msgs)
            (%kept-message-count entries cut-index)
            (cdr (assoc 'usage gen))
          ) ;make-pi-compaction-result
        ) ;let*
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
