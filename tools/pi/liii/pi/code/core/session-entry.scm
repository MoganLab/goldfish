;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-entry)
  (import (scheme base)
    (liii json)
    (liii list)
    (liii string)
    (liii pi ai message)
    (liii pi agent tools-compat)
  ) ;import
  (export pi-session-header-entry pi-session-header-entry?
    pi-session-header-entry-version pi-session-header-entry-id
    pi-session-header-entry-timestamp pi-session-header-entry-cwd
    pi-session-message-entry pi-session-message-entry?
    pi-session-message-entry-id pi-session-message-entry-parent-id
    pi-session-message-entry-timestamp pi-session-message-entry-message
    pi-session-compaction-entry pi-session-compaction-entry?
    pi-session-compaction-entry-id pi-session-compaction-entry-parent-id
    pi-session-compaction-entry-timestamp pi-session-compaction-entry-summary
    pi-session-compaction-entry-first-kept-entry-id
    pi-session-compaction-entry-tokens-before
    pi-session-compaction-entry-details pi-session-context-edit-entry
    pi-session-context-edit-entry? pi-session-context-edit-entry-id
    pi-session-context-edit-entry-parent-id
    pi-session-context-edit-entry-timestamp
    pi-session-context-edit-entry-target-id
    pi-session-context-edit-entry-replacement pi-session-state-entry
    pi-session-state-entry? pi-session-state-entry-type
    pi-session-state-entry-id pi-session-state-entry-parent-id
    pi-session-state-entry-timestamp pi-session-state-entry-fields
    pi-session-entry? pi-session-entry-type pi-session-entry-id
    pi-session-entry-parent-id pi-session-entry-timestamp pi-session-entry->json
    json->pi-session-entry
  ) ;export
  (begin

    (define-record-type <pi-session-header-entry>
      (%make-pi-session-header-entry version id timestamp cwd)
      pi-session-header-entry?
      (version pi-session-header-entry-version)
      (id pi-session-header-entry-id)
      (timestamp pi-session-header-entry-timestamp)
      (cwd pi-session-header-entry-cwd)
    ) ;define-record-type

    (define-record-type <pi-session-message-entry>
      (%make-pi-session-message-entry id parent-id timestamp message)
      pi-session-message-entry?
      (id pi-session-message-entry-id)
      (parent-id pi-session-message-entry-parent-id)
      (timestamp pi-session-message-entry-timestamp)
      (message pi-session-message-entry-message)
    ) ;define-record-type

    (define-record-type <pi-session-compaction-entry>
      (%make-pi-session-compaction-entry id parent-id timestamp summary
        first-kept-entry-id tokens-before details
      ) ;%make-pi-session-compaction-entry
      pi-session-compaction-entry?
      (id pi-session-compaction-entry-id)
      (parent-id pi-session-compaction-entry-parent-id)
      (timestamp pi-session-compaction-entry-timestamp)
      (summary pi-session-compaction-entry-summary)
      (first-kept-entry-id pi-session-compaction-entry-first-kept-entry-id)
      (tokens-before pi-session-compaction-entry-tokens-before)
      (details pi-session-compaction-entry-details)
    ) ;define-record-type

    (define-record-type <pi-session-context-edit-entry>
      (%make-pi-session-context-edit-entry id parent-id timestamp target-id
        replacement
      ) ;%make-pi-session-context-edit-entry
      pi-session-context-edit-entry?
      (id pi-session-context-edit-entry-id)
      (parent-id pi-session-context-edit-entry-parent-id)
      (timestamp pi-session-context-edit-entry-timestamp)
      (target-id pi-session-context-edit-entry-target-id)
      (replacement pi-session-context-edit-entry-replacement)
    ) ;define-record-type

    (define-record-type <pi-session-state-entry>
      (%make-pi-session-state-entry type id parent-id timestamp fields)
      pi-session-state-entry?
      (type pi-session-state-entry-type)
      (id pi-session-state-entry-id)
      (parent-id pi-session-state-entry-parent-id)
      (timestamp pi-session-state-entry-timestamp)
      (fields pi-session-state-entry-fields)
    ) ;define-record-type

    (define* (pi-session-header-entry id timestamp cwd (version 3))
      (%make-pi-session-header-entry version id timestamp cwd)
    ) ;define*

    (define (pi-session-message-entry id parent-id timestamp message)
      (%make-pi-session-message-entry id parent-id timestamp message)
    ) ;define

    (define* (pi-session-compaction-entry id
               parent-id
               timestamp
               summary
               first-kept-entry-id
               tokens-before
               (details '(()))
             ) ;pi-session-compaction-entry
      (%make-pi-session-compaction-entry id parent-id timestamp summary
        first-kept-entry-id tokens-before details
      ) ;%make-pi-session-compaction-entry
    ) ;define*

    (define* (pi-session-context-edit-entry id
               parent-id
               timestamp
               target-id
               (replacement #f)
             ) ;pi-session-context-edit-entry
      (%make-pi-session-context-edit-entry id parent-id timestamp target-id
        replacement
      ) ;%make-pi-session-context-edit-entry
    ) ;define*

    (define* (pi-session-state-entry type id parent-id timestamp (fields '()))
      (%make-pi-session-state-entry type id parent-id timestamp fields)
    ) ;define*

    (define (pi-session-entry? x)
      (or (pi-session-header-entry? x)
        (pi-session-message-entry? x)
        (pi-session-compaction-entry? x)
        (pi-session-context-edit-entry? x)
        (pi-session-state-entry? x)
      ) ;or
    ) ;define

    (define (pi-session-entry-type e)
      (cond ((pi-session-header-entry? e) "session")
            ((pi-session-message-entry? e) "message")
            ((pi-session-compaction-entry? e) "compaction")
            ((pi-session-context-edit-entry? e) "context_edit")
            ((pi-session-state-entry? e) (pi-session-state-entry-type e))
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-session-entry-id e)
      (cond ((pi-session-header-entry? e) (pi-session-header-entry-id e))
            ((pi-session-message-entry? e) (pi-session-message-entry-id e))
            ((pi-session-compaction-entry? e) (pi-session-compaction-entry-id e))
            ((pi-session-context-edit-entry? e) (pi-session-context-edit-entry-id e))
            ((pi-session-state-entry? e) (pi-session-state-entry-id e))
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-session-entry-parent-id e)
      (cond ((pi-session-header-entry? e) #f)
            ((pi-session-message-entry? e) (pi-session-message-entry-parent-id e))
            ((pi-session-compaction-entry? e) (pi-session-compaction-entry-parent-id e))
            ((pi-session-context-edit-entry? e) (pi-session-context-edit-entry-parent-id e))
            ((pi-session-state-entry? e) (pi-session-state-entry-parent-id e))
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-session-entry-timestamp e)
      (cond ((pi-session-header-entry? e) (pi-session-header-entry-timestamp e))
            ((pi-session-message-entry? e) (pi-session-message-entry-timestamp e))
            ((pi-session-compaction-entry? e) (pi-session-compaction-entry-timestamp e))
            ((pi-session-context-edit-entry? e) (pi-session-context-edit-entry-timestamp e))
            ((pi-session-state-entry? e) (pi-session-state-entry-timestamp e))
            (else #f)
      ) ;cond
    ) ;define

    (define (%json-parent-id pid)
      (if (or (not pid) (eq? pid 'null)) 'null pid)
    ) ;define

    (define (%json-entry? x)
      (and (pair? x) (not (pair? (car x))) (or (symbol? (car x)) (string? (car x))))
    ) ;define

    (define (%normalize-json-val v)
      (cond ((null? v) #())
            ((equal? v '(())) '(()))
            ((pi-message? v) (pi-message->json v))
            ((pi-block? v) (pi-block->json v))
            ((vector? v) (vector-map %normalize-json-val v))
            ((and (pair? v) (every %json-entry? v))
             (map
               (lambda (p)
                 (let ((k (if (symbol? (car p)) (symbol->string (car p)) (car p))))
                   (cons k (%normalize-json-val (cdr p)))
                 ) ;let
               ) ;lambda
               v
             ) ;map
            ) ;
            ((pair? v) (list->vector (map %normalize-json-val v)))
            (else v)
      ) ;cond
    ) ;define

    (define (%base-json-fields type id parent-id timestamp)
      (append (list (cons "type" type)
                (cons "id" id)
                (cons "parentId" (%json-parent-id parent-id))
              ) ;list
        (if timestamp (list (cons "timestamp" timestamp)) '())
      ) ;append
    ) ;define

    (define (pi-session-entry->json e)
      (cond
       ((pi-session-header-entry? e)
        (list (cons "type" "session")
          (cons "version" (pi-session-header-entry-version e))
          (cons "id" (pi-session-header-entry-id e))
          (cons "timestamp" (pi-session-header-entry-timestamp e))
          (cons "cwd" (pi-session-header-entry-cwd e))
        ) ;list
       ) ;
       ((pi-session-message-entry? e)
        (append (%base-json-fields "message"
                  (pi-session-message-entry-id e)
                  (pi-session-message-entry-parent-id e)
                  (pi-session-message-entry-timestamp e)
                ) ;%base-json-fields
          (list (cons "message" (%normalize-json-val (pi-session-message-entry-message e)))
          ) ;list
        ) ;append
       ) ;
       ((pi-session-compaction-entry? e)
        (let ((details (pi-session-compaction-entry-details e)))
          (append (%base-json-fields "compaction"
                    (pi-session-compaction-entry-id e)
                    (pi-session-compaction-entry-parent-id e)
                    (pi-session-compaction-entry-timestamp e)
                  ) ;%base-json-fields
            (list (cons "summary" (pi-session-compaction-entry-summary e))
              (cons "firstKeptEntryId"
                (let ((fid (pi-session-compaction-entry-first-kept-entry-id e)))
                  (if (or (not fid) (eq? fid 'null)) 'null fid)
                ) ;let
              ) ;cons
              (cons "tokensBefore" (pi-session-compaction-entry-tokens-before e))
              (cons "details"
                (if (or (not details) (null? details)) '(()) (%normalize-json-val details))
              ) ;cons
            ) ;list
          ) ;append
        ) ;let
       ) ;
       ((pi-session-context-edit-entry? e)
        (let ((rep (pi-session-context-edit-entry-replacement e)))
          (append (%base-json-fields "context_edit"
                    (pi-session-context-edit-entry-id e)
                    (pi-session-context-edit-entry-parent-id e)
                    (pi-session-context-edit-entry-timestamp e)
                  ) ;%base-json-fields
            (list (cons "targetId" (pi-session-context-edit-entry-target-id e))
              (cons "replacement"
                (if (or (not rep) (eq? rep 'null)) 'null (%normalize-json-val rep))
              ) ;cons
            ) ;list
          ) ;append
        ) ;let
       ) ;
       ((pi-session-state-entry? e)
        (append (%base-json-fields (pi-session-state-entry-type e)
                  (pi-session-state-entry-id e)
                  (pi-session-state-entry-parent-id e)
                  (pi-session-state-entry-timestamp e)
                ) ;%base-json-fields
          (%normalize-json-val (pi-session-state-entry-fields e))
        ) ;append
       ) ;
       (else e)
      ) ;cond
    ) ;define

    (define (%dict-ref obj key default-val)
      (if
        (not (and (pair? obj) (pair? (car obj))))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (let ((v (cdr pair))) (if (eq? v 'null) default-val v)) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%extra-state-fields obj)
      (filter
        (lambda (p)
          (and (pair? p)
            (let ((k (if (symbol? (car p)) (symbol->string (car p)) (car p))))
              (not (member k '("type" "id" "parentId" "timestamp")))
            ) ;let
          ) ;and
        ) ;lambda
        obj
      ) ;filter
    ) ;define

    (define (json->pi-session-entry obj)
      (cond ((pi-session-entry? obj) obj)
            ((not (and (pair? obj) (pair? (car obj)))) #f)
            (else
              (let ((etype (%dict-ref obj "type" #f))
                    (id (%dict-ref obj "id" #f))
                    (pid (%dict-ref obj "parentId" #f))
                    (ts (%dict-ref obj "timestamp" #f))
                   ) ;
                (cond ((not (string? etype)) #f)
                      ((string=? etype "session")
                       (pi-session-header-entry id
                         ts
                         (%dict-ref obj "cwd" "")
                         (%dict-ref obj "version" 3)
                       ) ;pi-session-header-entry
                      ) ;
                      ((string=? etype "message")
                       (let ((raw-msg (%dict-ref obj "message" #f)))
                         (pi-session-message-entry id pid ts (%maybe-json-msg raw-msg))
                       ) ;let
                      ) ;
                      ((string=? etype "compaction")
                       (pi-session-compaction-entry id
                         pid
                         ts
                         (%dict-ref obj "summary" "")
                         (%dict-ref obj "firstKeptEntryId" #f)
                         (%dict-ref obj "tokensBefore" 0)
                         (%dict-ref obj "details" '(()))
                       ) ;pi-session-compaction-entry
                      ) ;
                      ((string=? etype "context_edit")
                       (pi-session-context-edit-entry id
                         pid
                         ts
                         (%dict-ref obj "targetId" "")
                         (%dict-ref obj "replacement" #f)
                       ) ;pi-session-context-edit-entry
                      ) ;
                      (else (pi-session-state-entry etype id pid ts (%extra-state-fields obj)))
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
