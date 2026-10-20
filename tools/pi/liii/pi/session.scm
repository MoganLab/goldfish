;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi session)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii json)
    (liii uuid)
    (liii sort)
    (liii pi args)
    (liii pi types)
  ) ;import
  (export encode-cwd-to-session-dirname get-session-dir make-session-file-path
    read-session-history append-session-entry! find-latest-session-file
    save-session-header! save-session-message! setup-session-file
    setup-initial-messages persist-new-messages! find-last-session-message-id
    assoc
  ) ;export
  (begin

    (define (%init-new-session-file sdir cwd)
      (path-mkdir sdir :parents #t :exist-ok #t)
      (let* ((sid (uuid7))
             (now-iso (pi-iso-8601-utc-now))
             (fp (make-session-file-path sdir now-iso sid))
            ) ;
        (save-session-header! fp sid now-iso cwd)
        (values fp sdir)
      ) ;let*
    ) ;define

    (define (setup-session-file parsed cfg cwd)
      (if (pi-args-ref parsed 'no-session)
        (values #f #f)
        (let* ((agent-dir (cdr (assoc 'agent-dir cfg)))
               (sdir (get-session-dir agent-dir cwd))
              ) ;
          (if (pi-args-ref parsed 'continue)
            (let ((latest (find-latest-session-file sdir)))
              (if latest (values latest sdir) (%init-new-session-file sdir cwd))
            ) ;let
            (%init-new-session-file sdir cwd)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (setup-initial-messages parsed initial-msg sdir)
      (let* ((user-msg (if (not (string-null? initial-msg)) (list (pi-user-message initial-msg)) '())
             ) ;user-msg
            ) ;
        (if (pi-args-ref parsed 'continue)
          (let ((latest-file (find-latest-session-file sdir)))
            (if latest-file (append (read-session-history latest-file) user-msg) user-msg)
          ) ;let
          user-msg
        ) ;if
      ) ;let*
    ) ;define

    (define (%extract-id-from-line line)
      (let ((trimmed (string-trim line)))
        (if (<= (string-length trimmed) 0)
          #f
          (let ((parsed (catch #t (lambda () (string->json trimmed)) (lambda _ #f))))
            (and parsed (assoc "id" parsed) (cdr (assoc "id" parsed)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-last-session-message-id file-path)
      (let ((p-str (if (string? file-path) file-path (path->string file-path))))
        (if (not (file-exists? p-str))
          #f
          (let ((lines (path-read-lines p-str)) (last-id #f))
            (for-each
              (lambda (line)
                (let ((mid (%extract-id-from-line line)))
                  (when mid
                    (set! last-id mid)
                  ) ;when
                ) ;let
              ) ;lambda
              lines
            ) ;for-each
            last-id
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define* (persist-new-messages! sfile init-msgs all-final-msgs (persisted-count #f))
      (when sfile
        (let ((old-len (or persisted-count (length init-msgs)))
              (total-len (length all-final-msgs))
              (current-parent-id (find-last-session-message-id sfile))
             ) ;
          (when (> total-len old-len)
            (let loop
              ((rest (list-tail all-final-msgs old-len)))
              (when (pair? rest)
                (let* ((m (car rest))
                       (json-str (pi-serialize-event m))
                       (mid (uuid4))
                       (now-iso (pi-iso-8601-utc-now))
                      ) ;
                  (save-session-message! sfile json-str mid current-parent-id now-iso)
                  (set! current-parent-id mid)
                  (loop (cdr rest))
                ) ;let*
              ) ;when
            ) ;let
          ) ;when
        ) ;let
      ) ;when
    ) ;define*

    (define (encode-cwd-to-session-dirname cwd)
      (let* ((clean-cwd
               (if (and (> (string-length cwd) 0) (char=? (string-ref cwd 0) #\/))
                 (substring cwd 1 (string-length cwd))
                 cwd
               ) ;if
             ) ;clean-cwd
             (replaced (string-replace clean-cwd "/" "-"))
            ) ;
        (string-append "--" (string-replace replaced ":" "-") "--")
      ) ;let*
    ) ;define

    (define (get-session-dir agent-dir cwd)
      (let ((dirname (encode-cwd-to-session-dirname cwd)))
        (path->string (path-join agent-dir "sessions" dirname))
      ) ;let
    ) ;define

    (define (make-session-file-path session-dir timestamp-iso session-id)
      (let ((safe-time (string-replace (string-replace timestamp-iso ":" "-") "." "-")))
        (path->string (path-join session-dir (string-append safe-time "_" session-id ".jsonl"))
        ) ;path->string
      ) ;let
    ) ;define

    (define (append-session-entry! file-path entry-json-str)
      (let* ((p-str (if (string? file-path) file-path (path->string file-path)))
             (port (open-output-file p-str "a"))
            ) ;
        (display entry-json-str port)
        (newline port)
        (close-output-port port)
      ) ;let*
    ) ;define

    (define (save-session-header! file-path session-id timestamp-iso cwd)
      (let ((header (string-append "{\"type\":\"session\",\"version\":3,\"id\":\""
                      session-id "\",\"timestamp\":\"" timestamp-iso
                      "\",\"cwd\":\"" cwd "\"}"
                    ) ;string-append
            ) ;header
           ) ;
        (append-session-entry! file-path header)
      ) ;let
    ) ;define

    (define (save-session-message! file-path message-json-str id parent-id timestamp-iso)
      (let ((entry (string-append "{\"type\":\"message\",\"id\":\""
                     id
                     "\",\"parentId\":"
                     (if parent-id (string-append "\"" parent-id "\"") "null")
                     ",\"timestamp\":\""
                     timestamp-iso
                     "\",\"message\":"
                     message-json-str
                     "}"
                   ) ;string-append
            ) ;entry
           ) ;
        (append-session-entry! file-path entry)
      ) ;let
    ) ;define

    (define (%normalize-session-msg msg)
      (cond ((not (pair? msg)) msg)
            (else
              (map
                (lambda (entry)
                  (cond
                   ((and (pair? entry)
                      (or (equal? (car entry) "content") (eq? (car entry) 'content))
                      (vector? (cdr entry))
                    ) ;and
                    (cons (car entry) (vector->list (cdr entry)))
                   ) ;
                   (else entry)
                  ) ;cond
                ) ;lambda
                msg
              ) ;map
            ) ;else
      ) ;cond
    ) ;define

    (define (%extract-msg-from-line line)
      (let ((trimmed (string-trim line)))
        (if (<= (string-length trimmed) 0)
          #f
          (let ((parsed (catch #t (lambda () (string->json trimmed)) (lambda _ #f))))
            (and parsed
              (assoc "type" parsed)
              (string=? (cdr (assoc "type" parsed)) "message")
              (assoc "message" parsed)
              (%normalize-session-msg (cdr (assoc "message" parsed)))
            ) ;and
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (read-session-history file-path)
      (let ((p-str (if (string? file-path) file-path (path->string file-path))))
        (if (not (file-exists? p-str))
          '()
          (let ((lines (path-read-lines p-str)) (messages '()))
            (for-each
              (lambda (line)
                (let ((msg (%extract-msg-from-line line)))
                  (if msg (set! messages (cons msg messages)))
                ) ;let
              ) ;lambda
              lines
            ) ;for-each
            (reverse messages)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-latest-session-file session-dir)
      (let ((dir-str (if (string? session-dir) session-dir (path->string session-dir))))
        (if (not (file-exists? dir-str))
          #f
          (let ((files (path-glob dir-str "*.jsonl")))
            (if (null? files)
              #f
              (let* ((str-files
                       (map (lambda (f) (if (string? f) f (path->string f))) files)
                     ) ;str-files
                     (sorted (list-sort (lambda (a b) (string>? a b)) str-files))
                    ) ;
                (car sorted)
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
