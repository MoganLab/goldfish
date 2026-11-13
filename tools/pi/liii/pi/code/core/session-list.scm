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
    (scheme file)
    (liii string)
    (liii path)
    (liii sort)
    (liii list)
    (liii json)
    (liii unicode)
    (liii pi code cli args)
    (liii pi code core compaction serialize)
  ) ;import
  (export pi-read-session-info pi-list-session-infos pi-session-info-newer?
    make-pi-session-info pi-session-info? pi-session-info-path
    pi-session-info-id pi-session-info-cwd pi-session-info-name
    pi-session-info-created pi-session-info-modified
    pi-session-info-message-count pi-session-info-first-message
    pi-session-info-all-messages-text
  ) ;export
  (begin

    (define-record-type <pi-session-info>
      (make-pi-session-info path id cwd name created modified message-count
        first-message all-messages-text
      ) ;make-pi-session-info
      pi-session-info?
      (path pi-session-info-path)
      (id pi-session-info-id)
      (cwd pi-session-info-cwd)
      (name pi-session-info-name)
      (created pi-session-info-created)
      (modified pi-session-info-modified)
      (message-count pi-session-info-message-count)
      (first-message pi-session-info-first-message)
      (all-messages-text %pi-session-info-all-messages-text-raw
        %pi-session-info-all-messages-text-set!
      ) ;all-messages-text
    ) ;define-record-type

    (define (pi-session-info-all-messages-text info)
      (let ((val (%pi-session-info-all-messages-text-raw info)))
        (if (procedure? val)
          (let ((computed (val)))
            (%pi-session-info-all-messages-text-set! info computed)
            computed
          ) ;let
          val
        ) ;if
      ) ;let
    ) ;define

    (define (%ref object key default)
      (let ((pair (assoc key object)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%entry-type? entry type)
      (equal? (%ref entry "type" #f) type)
    ) ;define

    (define (%sanitize-text str)
      (if (string? str) (utf8-replace-invalid str) "")
    ) ;define

    ;; user/assistant 消息的投影文本；非文本或空文本消息返回 #f
    (define (%message-text entry)
      (let* ((message (%ref entry "message" '()))
             (role (%ref message "role" #f))
             (raw-text (pi-content->text (%ref message "content" "")))
            ) ;
        (and (member role '("user" "assistant"))
          (not (string-null? raw-text))
          (cons role (%sanitize-text raw-text))
        ) ;and
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

    (define (%scan-all-messages-port port)
      (let loop
        ((line (read-line port)) (header? #f) (acc '()))
        (if (eof-object? line)
          (string-join (reverse acc) " ")
          (cond ((string=? line "") (loop (read-line port) header? acc))
                ((not header?) (loop (read-line port) (and (%parse-line line) #t) acc))
                ((string-contains? line "\"type\":\"message\"")
                 (if (or (string-contains? line "\"role\":\"user\"")
                       (string-contains? line "\"role\":\"assistant\"")
                     ) ;or
                   (let* ((entry (%parse-line line))
                          (rt (and entry (%entry-type? entry "message") (%message-text entry)))
                         ) ;
                     (loop (read-line port) #t (if rt (cons (cdr rt) acc) acc))
                   ) ;let*
                   (loop (read-line port) #t acc)
                 ) ;if
                ) ;
                ((string-contains? line "\"type\":\"") (loop (read-line port) #t acc))
                (else
                  (let* ((entry (%parse-line line))
                         (rt (and entry (%entry-type? entry "message") (%message-text entry)))
                        ) ;
                    (loop (read-line port) #t (if rt (cons (cdr rt) acc) acc))
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;if
      ) ;let
    ) ;define

    ;; 仅在需要全文搜索时按需扫描 user/assistant 消息文本，
    ;; 跳过占大多数的 toolResult 与元数据行以降低解析开销
    (define (%read-all-messages-text path)
      (if (not (file-exists? path))
        ""
        (catch #t
          (lambda () (call-with-input-file path %scan-all-messages-port))
          (lambda _ "")
        ) ;catch
      ) ;if
    ) ;define

    (define (%extract-user-text entry)
      (let ((rt (%message-text entry)))
        (and rt (equal? (car rt) "user") (cdr rt))
      ) ;let
    ) ;define

    (define (%build-session-info path header last-info last-msg msg-count first-user)
      (let* ((info-entry (if (string? last-info) (%parse-line last-info) last-info))
             (raw-name (and info-entry (pi-normalize-session-name (%ref info-entry "name" #f)))
             ) ;raw-name
             (name (and (string? raw-name) (%sanitize-text raw-name)))
             (msg-entry (if (string? last-msg) (%parse-line last-msg) last-msg))
             (msg-ts (and msg-entry (%ref msg-entry "timestamp" #f)))
             (created (%sanitize-text (%ref header "timestamp" "")))
            ) ;
        (make-pi-session-info path
          (%sanitize-text (%ref header "id" ""))
          (%sanitize-text (%ref header "cwd" ""))
          name
          created
          (if (string? msg-ts) (%sanitize-text msg-ts) created)
          msg-count
          (or first-user "(no messages)")
          (lambda () (%read-all-messages-text path))
        ) ;make-pi-session-info
      ) ;let*
    ) ;define

    (define (%read-session-header port)
      (let loop
        ((line (read-line port)))
        (cond ((eof-object? line) #f)
              ((string=? line "") (loop (read-line port)))
              (else
                (let ((entry (%parse-line line)))
                  (cond ((not entry) (loop (read-line port)))
                        ((%entry-type? entry "session") entry)
                        (else #f)
                  ) ;cond
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%scan-session-port path port header)
      (let loop
        ((line (read-line port))
         (last-info #f)
         (last-msg #f)
         (msg-count 0)
         (first-user #f)
        ) ;
        (cond ((eof-object? line)
               (%build-session-info path header last-info last-msg msg-count first-user)
              ) ;
              ((string=? line "")
               (loop (read-line port) last-info last-msg msg-count first-user)
              ) ;
              ((string-contains? line "\"type\":\"message\"")
               (let ((next-first
                       (if (or first-user (not (string-contains? line "\"role\":\"user\"")))
                         first-user
                         (let ((entry (%parse-line line)))
                           (and entry (%entry-type? entry "message") (%extract-user-text entry))
                         ) ;let
                       ) ;if
                     ) ;next-first
                    ) ;
                 (loop (read-line port) last-info line (+ msg-count 1) next-first)
               ) ;let
              ) ;
              ((string-contains? line "\"type\":\"session_info\"")
               (loop (read-line port) line last-msg msg-count first-user)
              ) ;
              ((string-contains? line "\"type\":\"")
               (loop (read-line port) last-info last-msg msg-count first-user)
              ) ;
              (else
                (let ((entry (%parse-line line)))
                  (cond ((not entry) (loop (read-line port) last-info last-msg msg-count first-user))
                        ((%entry-type? entry "message")
                         (loop (read-line port)
                           last-info
                           entry
                           (+ msg-count 1)
                           (or first-user (%extract-user-text entry))
                         ) ;loop
                        ) ;
                        ((%entry-type? entry "session_info")
                         (loop (read-line port) entry last-msg msg-count first-user)
                        ) ;
                        (else (loop (read-line port) last-info last-msg msg-count first-user))
                  ) ;cond
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-read-session-info file-path)
      (let ((path (if (string? file-path) file-path (path->string file-path))))
        (and (file-exists? path)
          (catch #t
            (lambda ()
              (call-with-input-file path
                (lambda (port)
                  (let ((header (%read-session-header port)))
                    (and header (%scan-session-port path port header))
                  ) ;let
                ) ;lambda
              ) ;call-with-input-file
            ) ;lambda
            (lambda _ #f)
          ) ;catch
        ) ;and
      ) ;let
    ) ;define

    ;; 会话摘要的最近活动序（modified 降序，path 升序打破平局），
    ;; 供列表枚举与选择器 recent 排序共用，保证两处口径一致
    (define (pi-session-info-newer? a b)
      (let ((am (pi-session-info-modified a)) (bm (pi-session-info-modified b)))
        (if (equal? am bm)
          (string<? (pi-session-info-path a) (pi-session-info-path b))
          (string>? am bm)
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
        (list-sort pi-session-info-newer? sessions)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
