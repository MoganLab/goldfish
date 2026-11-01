(import (scheme base)
  (scheme file)
  (scheme write)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii list)
  (liii os)
  (liii path)
  (liii json)
  (liii shutil)
  (liii subprocess)
  (liii sys)
  (liii pi ai message)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core session-manager)
  (liii pi code core compaction session)
  (liii pi code core session-projection)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

;; [1205] 恢复路径与手动 compact 继续路径接入投影入口 pi-build-session-projection：
;; - %session-context（--continue / --resume / TUI worker 复用会话文件）
;; - run-compact-flow 在压缩写入条目后的重新投影
;; 由于两者均为内部过程，本测试经由公开入口 run-session-flow /
;; run-compact-flow + 可注入 request-fn 的 run-agent-loop-with-timeout 间接覆盖：
;; 用最小 mock cfg 驱动真实流程，检查传给模型的初始上下文与会话文件落盘。

(define GF-BIN (executable))

(define STUB
  (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
    "tools/pi/tests/stub_gemini_server.py"
    (if (file-exists? "tests/stub_gemini_server.py")
      "tests/stub_gemini_server.py"
      "../../tools/pi/tests/stub_gemini_server.py"
    ) ;if
  ) ;if
) ;define

(define FIXTURE-DIR
  (if (file-exists? "tools/pi/tests/fixtures/direct")
    "tools/pi/tests/fixtures/direct"
    (if (file-exists? "tests/fixtures/direct")
      "tests/fixtures/direct"
      "../../tools/pi/tests/fixtures/direct"
    ) ;if
  ) ;if
) ;define

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-1205-projection-wire-modes"))
) ;define

(define E2E-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-1205-compact-e2e-agent"))
) ;define

(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR)
) ;when
(when (file-exists? E2E-AGENT-DIR)
  (rmtree E2E-AGENT-DIR)
) ;when

(define %captured (vector #f))

(define (project-messages sfile)
  (let*-values (((msgs _state _diag)
                 (pi-build-session-projection (pi-read-session-entries sfile))
                ) ;
               ) ;
    msgs
  ) ;let*-values
) ;define

(define (append-manual-compaction! sfile summary first-kept-id)
  (append-session-entry! sfile
    (string-append "{\"type\":\"compaction\",\"id\":\"c-manual-1\","
      "\"parentId\":"
      (json->string first-kept-id)
      ",\"summary\":"
      (json->string summary)
      ",\"firstKeptEntryId\":"
      (json->string first-kept-id)
      ",\"tokensBefore\":100}"
    ) ;string-append
  ) ;append-session-entry!
) ;define

(define (read-all p . maybe-stream)
  (let ((stream (if (null? maybe-stream) 'stdout (car maybe-stream))))
    (let loop
      ((acc ""))
      (let ((c (process-read p :stream stream)))
        (if (eof-object? c) acc (loop (string-append acc c)))
      ) ;let
    ) ;let
  ) ;let
) ;define

(path-mkdir E2E-AGENT-DIR :parents #t :exist-ok #t)
(for-each
  (lambda (name)
    (path-copy (path-join (path FIXTURE-DIR) (path name))
      (path-join (path E2E-AGENT-DIR) (path name))
    ) ;path-copy
  ) ;lambda
  '("settings.json" "auth.json" "providers.json")
) ;for-each

(let* ((proc (process-spawn (list "python3" STUB)
               :stdin  'null
               :stdout 'pipe
               :stderr 'discard
             ) ;process-spawn
       ) ;proc
       (first-line (process-read-line proc))
       (port
         (if (and (string? first-line) (string-starts? first-line "PORT:"))
           (string->number (string-trim-both (substring first-line 5 (string-length first-line)))
           ) ;string->number
           #f
         ) ;if
       ) ;port
       (base-url (and port (string-append "http://127.0.0.1:" (number->string port))))
      ) ;
  (check-true (number? port))
  (putenv "GF_PI_BASE_URL" base-url)
  (putenv "GF_PI_MODEL" "gemini-3.8-flash")
  (putenv "https_proxy" "")
  (putenv "http_proxy" "")
  (putenv "all_proxy" "")
  (putenv "ALL_PROXY" "")
  (putenv "HTTPS_PROXY" "")
  (putenv "HTTP_PROXY" "")
  (putenv "no_proxy" "")
  (putenv "NO_PROXY" "")

  (define (make-mock-cfg agent-dir)
    (list (cons 'agent-dir agent-dir)
      (cons 'tools '())
      (cons 'provider "google")
      (cons 'api-key "mock-key")
      (cons 'model-id "gemini-3.8-flash")
      (cons 'model-meta (list (cons 'base-url base-url)))
      (cons 'settings '((max-retries . 0)))
      (cons 'proxy-url #f)
    ) ;list
  ) ;define

  (dynamic-wind
    (lambda () #f)
    (lambda ()
      ;; ---- 用例 1：run-session-flow 恢复压缩会话 --------------------------------
      (let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)
                        (cons "GF_PI_BASE_URL" base-url)
                  ) ;list
             ) ;env
             (parsed (parse-pi-args '()))
             (cfg (resolve-pi-config parsed env))
             (cwd (path->string (path-cwd)))
            ) ;
        (let*-values (((sfile _sdir) (setup-session-file parsed cfg cwd)))
          ;; 历史消息 m1 -> m2，随后手工压缩：保留 m2
          (persist-new-messages! sfile
            '()
            (list (pi-user-message "question-1") (pi-user-message "question-2"))
            0
          ) ;persist-new-messages!
          (let* ((entries (pi-read-session-entries sfile))
                 (m2-id
                   (cdr
                     (assoc "id"
                       (car
                         (reverse
                           (filter
                             (lambda (e) (equal? (cdr (assoc "type" e)) "message"))
                             entries
                           ) ;filter
                         ) ;reverse
                       ) ;car
                     ) ;assoc
                   ) ;cdr
                 ) ;m2-id
                ) ;
            (append-manual-compaction! sfile "历史已压缩为这句摘要" m2-id)
            ;; TUI worker 复用会话文件的路径：session-file 传入时携带既有上下文
            (run-session-flow parsed
              (make-mock-cfg TMP-AGENT-DIR)
              "new-question"
              #f
              (lambda (res-msgs) (vector-set! %captured 0 res-msgs))
              sfile
              '()
              #f
            ) ;run-session-flow
            ;; 会话文件投影：首条为摘要用户消息，其后 m2、新用户消息、assistant 回复
            (let* ((msgs (project-messages sfile))
                   (contents (map pi-user-message-content (filter pi-user-message? msgs)))
                  ) ;
              (check (length msgs) => 4)
              (check-true (string-contains? (car contents) "历史已压缩为这句摘要"))
              (check-true (string-starts? (car contents) "The conversation history"))
              (check (cadr contents) => "question-2")
              (check (caddr contents) => "new-question")
              ;; 摘要消息本身不落盘：文件中 message 条目为 3 条（q1、q2、new-question；
              ;; assistant 回复同样落盘为第 4 条）
              (check
                (length
                  (filter
                    (lambda (e) (equal? (cdr (assoc "type" e)) "message"))
                    (pi-read-session-entries sfile)
                  ) ;filter
                ) ;length
                =>
                4
              ) ;check
            ) ;let*
          ) ;let*
        ) ;let*-values
      ) ;let*

      ;; ---- 用例 2：--continue 恢复路径 ------------------------------------------
      (let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)
                        (cons "GF_PI_BASE_URL" base-url)
                  ) ;list
             ) ;env
             (cwd (path->string (path-cwd)))
             (parsed-cont (parse-pi-args '("--continue")))
            ) ;
        (let*-values (((sfile-new _sdir)
                       (setup-session-file (parse-pi-args '())
                         (resolve-pi-config (parse-pi-args '()) env)
                         cwd
                       ) ;setup-session-file
                      ) ;
                     ) ;
          (persist-new-messages! sfile-new
            '()
            (list (pi-user-message "早期问题-甲"))
            0
          ) ;persist-new-messages!
          ;; --continue 恢复目录中最新会话（即刚写入的 sfile-new）
          (run-session-flow parsed-cont
            (make-mock-cfg TMP-AGENT-DIR)
            "继续提问"
            #f
            (lambda _ #f)
            #f
            '()
            #f
          ) ;run-session-flow
          (let* ((msgs (project-messages sfile-new))
                 (contents (map pi-user-message-content (filter pi-user-message? msgs)))
                ) ;
            (check (length contents) => 2)
            (check (car contents) => "早期问题-甲")
            (check (cadr contents) => "继续提问")
          ) ;let*
        ) ;let*-values
      ) ;let*

      ;; ---- 用例 3：手动 compact 继续路径（run-compact-flow，stub 服务器 e2e） ----
      (let* ((test-env (append (get-environment-variables)
                         (list (cons "PI_CODING_AGENT_DIR" E2E-AGENT-DIR)
                           (cons "GF_PI_BASE_URL" base-url)
                           (cons "GF_PI_MODEL" "gemini-3.8-flash")
                           (cons "https_proxy" "")
                           (cons "http_proxy" "")
                           (cons "all_proxy" "")
                           (cons "ALL_PROXY" "")
                           (cons "HTTPS_PROXY" "")
                           (cons "HTTP_PROXY" "")
                           (cons "no_proxy" "")
                           (cons "NO_PROXY" "")
                           (cons "ANTHROPIC_BASE_URL" "")
                           (cons "ANTHROPIC_API_KEY" "")
                           (cons "ANTHROPIC_AUTH_TOKEN" "")
                           (cons "DEEPSEEK_API_KEY" "")
                           (cons "DEEPSEEK_BASE_URL" "")
                         ) ;list
                       ) ;append
             ) ;test-env
             (sdir (get-session-dir E2E-AGENT-DIR (path->string (path-cwd))))
             (sfile (path->string (path-join sdir "2026-10-29T00-00-00-000Z-compact.jsonl")))
            ) ;
        (path-mkdir sdir :parents #t :exist-ok #t)
        (save-session-header! sfile
          "compact-1205"
          "2026-10-29T00:00:00.000Z"
          (path->string (path-cwd))
        ) ;save-session-header!
        ;; 4 条消息：前两条超长保证超过 keepRecentTokens（默认 20000），
        ;; 摘要请求会压缩它们，保留后两条（对齐 pi-compact-e2e-test 的做法）
        (persist-new-messages! sfile
          '()
          (list (pi-user-message (string-append "早期问题一：" (make-string 100000 #\a)))
            (pi-assistant-message
              (list (pi-text-block (string-append "早期回答一：" (make-string 100000 #\b)))
              ) ;list
              :stop-reason "stop"
            ) ;pi-assistant-message
            (pi-user-message "近期问题二")
            (pi-assistant-message (list (pi-text-block "近期回答二"))
              :stop-reason "stop"
            ) ;pi-assistant-message
          ) ;list
          0
        ) ;persist-new-messages!
        ;; --compact 带 prompt：压缩后继续本次请求
        (let* ((p (process-spawn (list GF-BIN "pi" "--compact" "压缩后追问")
                    :env    test-env
                    :stdin  'null
                    :stdout 'pipe
                    :stderr 'pipe
                  ) ;process-spawn
               ) ;p
               (out (read-all p 'stdout))
               (err (read-all p 'stderr))
               (code (process-wait p 30000))
               (entries (pi-read-session-entries sfile))
               (msg-entries
                 (filter
                   (lambda (e) (equal? (cdr (assoc "type" e)) "message"))
                   entries
                 ) ;filter
               ) ;msg-entries
               (compaction-entries
                 (filter
                   (lambda (e) (equal? (cdr (assoc "type" e)) "compaction"))
                   entries
                 ) ;filter
               ) ;compaction-entries
              ) ;
          (check-true (= code 0))
          ;; 带 prompt 时压缩摘要写 stderr，stdout 为模型回复（Hello stream!）
          (check-true (string-contains? err "Compacted session"))
          (check-true (string-contains? out "Hello stream!"))
          ;; 压缩条目已写入
          (check (length compaction-entries) => 1)
          ;; 投影：首条为摘要用户消息，其后保留消息 + 压缩后追加的新用户消息与回复
          (let*-values (((proj-msgs _state _diag) (pi-build-session-projection entries)))
            (check-true (pair? proj-msgs))
            (check (pi-message-role (car proj-msgs)) => "user")
            (check-true (string-starts? (pi-user-message-content (car proj-msgs))
                          "The conversation history"
                        ) ;string-starts?
            ) ;check-true
            ;; 最后两条：新用户消息在投影结果之后、assistant 回复在其后
            (check (pi-message-role (cadr (reverse proj-msgs))) => "user")
            (check (pi-user-message-content (cadr (reverse proj-msgs)))
              =>
              "压缩后追问"
            ) ;check
            (check (pi-message-role (car (reverse proj-msgs))) => "assistant")
            ;; 摘要消息不落盘：文件中 message 条目 = 4 条历史 + 1 新用户 + 1 回复
            (check (length msg-entries) => 6)
            ;; 文件中不存在摘要用户消息的落盘痕迹
            (check-false (string-contains? (path-read-text sfile)
                           "The conversation history before this point"
                         ) ;string-contains?
            ) ;check-false
          ) ;let*-values
          (process-close p)
        ) ;let*
      ) ;let*
    ) ;lambda
    (lambda ()
      (process-kill proc)
      (process-close proc)
      (when (file-exists? TMP-AGENT-DIR) (rmtree TMP-AGENT-DIR #t))
      (when (file-exists? E2E-AGENT-DIR) (rmtree E2E-AGENT-DIR #t))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
