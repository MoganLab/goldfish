;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; --resume 会话枚举：从 JSONL 提取选择器所需摘要，不投影完整对话上下文。
(define-library (liii pi code core session-list)
  (import (scheme base)
    (liii string)
    (liii path)
    (liii json)
    (liii sort)
    (liii list)
  ) ;import
  (export pi-read-session-info pi-list-session-infos)
  (begin

    (define (%ref object key default)
      (let ((pair (assoc key object)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%block-text block)
      (if (and (pair? block)
            (equal? (%ref block "type" #f) "text")
            (string? (%ref block "text" #f))
          ) ;and
        (%ref block "text" "")
        ""
      ) ;if
    ) ;define

    (define (%content-text content)
      (cond ((string? content) content)
            ((vector? content) (%content-text (vector->list content)))
            ((pair? content) (apply string-append (map %block-text content)))
            (else "")
      ) ;cond
    ) ;define

    (define (%parse-line line)
      (catch #t (lambda () (string->json (string-trim line))) (lambda args #f))
    ) ;define

    (define (pi-read-session-info file-path)
      (let ((path (if (string? file-path) file-path (path->string file-path))))
        (if (not (file-exists? path))
          #f
          (let ((header #f)
                (invalid? #f)
                (name #f)
                (first-message #f)
                (texts '())
                (message-count 0)
                (modified #f)
               ) ;
            (for-each
              (lambda (line)
                (let ((entry (%parse-line line)))
                  (when entry
                    (if (not header)
                      (if (equal? (%ref entry "type" #f) "session")
                        (set! header entry)
                        (set! invalid? #t)
                      ) ;if
                      (let ((type (%ref entry "type" #f)))
                        (cond
                         ((equal? type "session_info")
                          (let ((value (%ref entry "name" #f)))
                            (set! name
                              (and (string? value)
                                (not (string-null? (string-trim-both value)))
                                (string-trim-both value)
                              ) ;and
                            ) ;set!
                          ) ;let
                         ) ;
                         ((equal? type "message")
                          (set! message-count (+ message-count 1))
                          (let ((timestamp (%ref entry "timestamp" #f)))
                            (when (string? timestamp)
                              (set! modified timestamp)
                            ) ;when
                          ) ;let
                          (let* ((message (%ref entry "message" '()))
                                 (role (%ref message "role" #f))
                                 (text (%content-text (%ref message "content" "")))
                                ) ;
                            (when (and (member role '("user" "assistant")) (not (string-null? text)))
                              (set! texts (cons text texts))
                              (when (and (equal? role "user") (not first-message))
                                (set! first-message text)
                              ) ;when
                            ) ;when
                          ) ;let*
                         ) ;
                        ) ;cond
                      ) ;let
                    ) ;if
                  ) ;when
                ) ;let
              ) ;lambda
              (path-read-lines path)
            ) ;for-each
            (and header
              (not invalid?)
              (list (cons 'path path)
                (cons 'id (%ref header "id" ""))
                (cons 'cwd (%ref header "cwd" ""))
                (cons 'name name)
                (cons 'created (%ref header "timestamp" ""))
                (cons 'modified (or modified (%ref header "timestamp" "")))
                (cons 'message-count message-count)
                (cons 'first-message (or first-message "(no messages)"))
                (cons 'all-messages-text (string-join (reverse texts) " "))
              ) ;list
            ) ;and
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-list-session-infos session-dir . recursive-opt)
      (let* ((dir (if (string? session-dir) session-dir (path->string session-dir)))
             (recursive? (and (pair? recursive-opt) (car recursive-opt)))
             (files (if (file-exists? dir) ((if recursive? path-rglob path-glob) dir "*.jsonl") '())
             ) ;files
             (sessions (filter (lambda (info) info) (map pi-read-session-info files)))
            ) ;
        (list-sort
          (lambda (a b)
            (let ((am (cdr (assoc 'modified a))) (bm (cdr (assoc 'modified b))))
              (if (equal? am bm)
                (string<? (cdr (assoc 'path a)) (cdr (assoc 'path b)))
                (string>? am bm)
              ) ;if
            ) ;let
          ) ;lambda
          sessions
        ) ;list-sort
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
