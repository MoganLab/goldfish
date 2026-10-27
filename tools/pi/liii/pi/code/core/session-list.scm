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
    (liii sort)
    (liii list)
    (liii pi code cli args)
    (liii pi code core compaction session)
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
      (all-messages-text pi-session-info-all-messages-text)
    ) ;define-record-type

    (define (%ref object key default)
      (let ((pair (assoc key object)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%entry-type? entry type)
      (equal? (%ref entry "type" #f) type)
    ) ;define

    ;; user/assistant 消息的投影文本；非文本或空文本消息返回 #f
    (define (%message-text entry)
      (let* ((message (%ref entry "message" '()))
             (role (%ref message "role" #f))
             (text (pi-content->text (%ref message "content" "")))
            ) ;
        (and (member role '("user" "assistant"))
          (not (string-null? text))
          (cons role text)
        ) ;and
      ) ;let*
    ) ;define

    (define (%summarize-session path header entries)
      (let* ((infos (filter (lambda (e) (%entry-type? e "session_info")) entries))
             (name
               (and (pair? infos) (pi-normalize-session-name (%ref (last infos) "name" #f)))
             ) ;name
             (messages (filter (lambda (e) (%entry-type? e "message")) entries))
             (timestamps
               (filter string? (map (lambda (e) (%ref e "timestamp" #f)) messages))
             ) ;timestamps
             (role-texts (filter (lambda (rt) rt) (map %message-text messages)))
             (first-user
               (find (lambda (rt) (equal? (car rt) "user")) role-texts)
             ) ;first-user
            ) ;
        (make-pi-session-info path
          (%ref header "id" "")
          (%ref header "cwd" "")
          name
          (%ref header "timestamp" "")
          (if (pair? timestamps) (last timestamps) (%ref header "timestamp" ""))
          (length messages)
          (if first-user (cdr first-user) "(no messages)")
          (string-join (map cdr role-texts) " ")
        ) ;make-pi-session-info
      ) ;let*
    ) ;define

    (define (pi-read-session-info file-path)
      (let ((path (if (string? file-path) file-path (path->string file-path))))
        (and (file-exists? path)
          (let ((entries (pi-read-session-entries path)))
            (and (pair? entries)
              (%entry-type? (car entries) "session")
              (%summarize-session path (car entries) (cdr entries))
            ) ;and
          ) ;let
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
