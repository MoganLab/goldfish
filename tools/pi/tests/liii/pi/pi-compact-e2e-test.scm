(import (scheme process-context)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii subprocess)
  (liii sys)
  (liii pi)
  (liii pi session)
  (liii pi compact)
) ;import

(check-set-mode! 'report-failed)

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define STUB
  (if (file-exists? "tests/stub_gemini_server.py")
    "tests/stub_gemini_server.py"
    "tools/pi/tests/stub_gemini_server.py"
  ) ;if
) ;define

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/direct")
    "tests/fixtures/direct"
    "tools/pi/tests/fixtures/direct"
  ) ;if
) ;define

(define GF_BIN (executable))

(define AGENT_DIR (path->string (path-join (path-temp-dir) "pi-compact-e2e-agent")))

(define (read-all p . maybe-stream)
  (let ((stream (if (null? maybe-stream) 'stdout (car maybe-stream))))
    (let loop ((acc ""))
      (let ((c (process-read p :stream stream)))
        (if (eof-object? c) acc (loop (string-append acc c)))))))

(define (setup-agent-dir)
  (path-mkdir (path AGENT_DIR) :parents #t :exist-ok #t)
  (for-each
    (lambda (name)
      (path-copy (path-join (path FIXTURE_DIR) (path name))
        (path-join (path AGENT_DIR) (path name))
      ) ;path-copy
    ) ;lambda
    '("settings.json" "auth.json" "models-store.json")
  ) ;for-each
) ;define

(define (write-large-session)
  (let* ((sdir (get-session-dir AGENT_DIR (path->string (path-cwd))))
         (sfile (path->string (path-join sdir "2026-10-19T00-00-00-000Z_compact-e2e.jsonl"))
         ) ;sfile
        ) ;
    (path-mkdir sdir :parents #t :exist-ok #t)
    (save-session-header! sfile
      "compact-e2e"
      "2026-10-19T00:00:00.000Z"
      (path->string (path-cwd))
    ) ;save-session-header!
    (let ((msgs
            (list (pi-user-message (make-string 100000 #\a))
              (pi-assistant-message (list (pi-text-block (make-string 100000 #\b)))
                :stop-reason "stop"
              ) ;pi-assistant-message
              (pi-user-message "next question")
              (pi-assistant-message (list (pi-text-block "done")) :stop-reason "stop")
            ) ;list
          ) ;msgs
          (parent #f)
         ) ;
      (for-each
        (lambda (m)
          (let ((id (uuid4)))
            (save-session-message! sfile
              (pi-serialize-event (pi-message->alist m))
              id
              parent
              (pi-iso-8601-utc-now)
            ) ;save-session-message!
            (set! parent id)
          ) ;let
        ) ;lambda
        msgs
      ) ;for-each
    ) ;let
    sfile
  ) ;let*
) ;define

(when (file-exists? AGENT_DIR)
  (path-rmdir (path AGENT_DIR))
) ;when
(setup-agent-dir)

(define session-file (write-large-session))

(let* ((proc (process-spawn (list "python3" STUB)
               :stdin  'null
               :stdout 'pipe
               :stderr 'discard
             ) ;process-spawn
       ) ;proc
       (port (parse-port-line (process-read-line proc)))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
       (test-env (append (list (cons "PI_CODING_AGENT_DIR" AGENT_DIR)
                           (cons "GF_PI_BASE_URL" base-url)
                           (cons "GF_PI_MODEL" "gemini-3.8-flash")
                           (cons "https_proxy" "")
                           (cons "http_proxy" "")
                           (cons "all_proxy" "")
                           (cons "ALL_PROXY" "")
                           (cons "HTTPS_PROXY" "")
                           (cons "HTTP_PROXY" "")
                         ) ;list
                         (get-environment-variables)
                 ) ;append
       ) ;test-env
      ) ;
  (check-true (number? port))

  ;; 1. --compact 不带 prompt：只压缩并退出
  (let* ((p (process-spawn (list GF_BIN "pi" "--compact")
              :env    test-env
              :stdin  'null
              :stdout 'pipe
              :stderr 'pipe
            ) ;process-spawn
         ) ;p
         (out (read-all p 'stdout))
         (err (read-all p 'stderr))
         (code (process-wait p 30000))
         (entries (pi-read-session-entries session-file))
         (messages
           (filter
             (lambda (e) (equal? (cdr (assoc "type" e)) "message"))
             entries
           ) ;filter
         ) ;messages
         (last-entry (car (reverse entries)))
        ) ;
    (check code => 0)
    (check-true (string-contains? out "Compacted session"))
    (check-true (string-contains? err "Compacted session"))
    (check (cdr (assoc "type" last-entry)) => "compaction")
    (check-true (string? (cdr (assoc "firstKeptEntryId" last-entry))))
    (check-true (if (assoc "firstKeptEntryId" last-entry) #t #f))
    ;; 原文一条不少：4 条 message 仍在
    (check (length messages) => 4)
    (process-close p)
  ) ;let*

  ;; 2. --compact 带 prompt：压缩后继续本次请求
  (let* ((p (process-spawn (list GF_BIN "pi" "--compact" "follow up")
              :env    test-env
              :stdin  'null
              :stdout 'pipe
              :stderr 'pipe
            ) ;process-spawn
         ) ;p
         (out (read-all p 'stdout))
         (err (read-all p 'stderr))
         (code (process-wait p 30000))
         (entries (pi-read-session-entries session-file))
         (messages
           (filter
             (lambda (e) (equal? (cdr (assoc "type" e)) "message"))
             entries
           ) ;filter
         ) ;messages
        ) ;
    (check code => 0)
    (check-true (string-contains? out "Hello stream!"))
    ;; 新增 user prompt 与 assistant 回复
    (check-true (> (length messages) 4))
    (process-close p)
  ) ;let*

  ;; 3. -c 续接压缩后的会话：基于 summary + 保留消息
  (let* ((p (process-spawn (list GF_BIN "pi" "-c" "-p" "continue after compact")
              :env    test-env
              :stdin  'null
              :stdout 'pipe
              :stderr 'pipe
            ) ;process-spawn
         ) ;p
         (out (read-all p 'stdout))
         (code (process-wait p 30000))
        ) ;
    (check code => 0)
    (check-true (string-contains? out "Hello stream!"))
    (process-close p)
  ) ;let*

  ;; 4. 阈值自动压缩：reserveTokens 大于 contextWindow 时必然触发
  (path-write-text (path-join (path AGENT_DIR) (path "settings.json"))
    "{\"defaultProvider\":\"google\",\"defaultModel\":\"gemini-3.8-flash\",\"compaction\":{\"enabled\":true,\"reserveTokens\":2000000,\"keepRecentTokens\":20000}}"
  ) ;path-write-text
  (let* ((p (process-spawn (list GF_BIN "pi" "-c" "-p" "auto compact please")
              :env    test-env
              :stdin  'null
              :stdout 'pipe
              :stderr 'pipe
            ) ;process-spawn
         ) ;p
         (err (read-all p 'stderr))
         (code (process-wait p 30000))
        ) ;
    (check code => 0)
    (check-true (string-contains? err "Auto-compacted session"))
    (process-close p)
  ) ;let*

  (process-kill proc)
  (process-close proc)
) ;let*

(check-report)
