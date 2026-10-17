;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldpi-session)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii json)
    (liii time)
    (liii sort)
  ) ;import
  (export encode-cwd-to-session-dirname get-session-dir make-session-file-path
    read-session-history append-session-entry! find-latest-session-file
    save-session-header! save-session-message!
  ) ;export
  (begin

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

    (define (%extract-msg-from-line line)
      (let ((trimmed (string-trim line)))
        (if (<= (string-length trimmed) 0)
          #f
          (let ((parsed (catch #t (lambda () (string->json trimmed)) (lambda _ #f))))
            (and parsed
              (assoc "type" parsed)
              (string=? (cdr (assoc "type" parsed)) "message")
              (assoc "message" parsed)
              (cdr (assoc "message" parsed))
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
