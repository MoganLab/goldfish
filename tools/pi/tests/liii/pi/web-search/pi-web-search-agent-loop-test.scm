(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii list)
  (liii json)
  (liii sys)
  (liii path)
  (liii subprocess)
  (liii pi)
)

(check-set-mode! 'report-failed)

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  )
)

(define stub-server-file
  (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
    "tools/pi/tests/stub_gemini_server.py"
    (if (file-exists? "tests/stub_gemini_server.py")
      "tests/stub_gemini_server.py"
      "../../tools/pi/tests/stub_gemini_server.py"
    )
  )
)

(define FIXTURE_DIR
  (if (file-exists? "tools/pi/tests/fixtures/direct")
    "tools/pi/tests/fixtures/direct"
    (if (file-exists? "tests/fixtures/direct")
      "tests/fixtures/direct"
      "../../tools/pi/tests/fixtures/direct"
    )
  )
)

(define GF_BIN (executable))

(define %tmp-agent-dir
  (path->string (path-join (path-temp-dir) "gf-pi-websearch-e2e-direct"))
)

(define (setup-fixtures!)
  (unless (path-exists? (path %tmp-agent-dir))
    (path-mkdir (path %tmp-agent-dir) :parents #t :exist-ok #t)
  ) ;unless
  (for-each
    (lambda (name)
      (let ((src (path-join (path FIXTURE_DIR) (path name)))
            (dst (path-join (path %tmp-agent-dir) (path name)))
           ) ;
        (when (path-exists? src)
          (path-copy src dst)
        ) ;when
      ) ;let
    ) ;lambda
    '("auth.json" "models-store.json" "models.json" "settings.json")
  ) ;for-each
) ;define

(define (cleanup-fixtures!)
  (when (path-exists? (path %tmp-agent-dir))
    (g_rmtree %tmp-agent-dir)
  ) ;when
) ;define

(define (read-all p . maybe-stream)
  (let ((stream (if (null? maybe-stream) 'stdout (car maybe-stream))))
    (let loop ((acc ""))
      (let ((c (process-read p :stream stream)))
        (if (eof-object? c) acc (loop (string-append acc c)))))))

(define (split-json-lines text)
  (filter (lambda (s) (not (string-null? s)))
    (map string-trim-both (string-split text "\n"))))

(setup-fixtures!)

(dynamic-wind
  (lambda () #f)
  (lambda ()
    (let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
           (port-line (process-read-line proc))
           (port (parse-port-line port-line))
           (base-url (string-append "http://127.0.0.1:" (number->string port)))
           (test-env (append (get-environment-variables)
                       (list (cons "PI_CODING_AGENT_DIR" %tmp-agent-dir)
                         (cons "GF_PI_BASE_URL" base-url)
                         (cons "GOOGLE_BASE_URL" base-url)
                         (cons "GF_PI_MODEL" "agent-web-search")
                         (cons "GOOGLE_SEARCH_MODEL" "web-search-basic")
                         (cons "GOOGLE_API_KEY" "test-key-search")
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
          ) ;
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (check-true (number? port))

  ;; 1. print 模式：web_search 闭环调用与 Sources 引用展示
  (let* ((cmd (list GF_BIN "pi" "--model" "agent-web-search" "--tools" "+web_search"
                "--print" "Search goldfish scheme"
              )
         )
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 15000))
         (out (process-read p :stream 'stdout))
         (err (process-read p :stream 'stderr))
        )
    (check code => 0)
    ;; 验证 stdout 包含了基于搜索的最终答案与 Sources 引用
    (check-true (string-contains? out "Goldfish Scheme is a minimalist Scheme implementation"))
    (check-true (string-contains? out "## Sources"))
    (check-true (string-contains? out "https://goldfish.liii.pro"))
    ;; 验证 stderr 输出了执行搜索的提示
    (check-true (string-contains? err "Searching web: Goldfish Scheme..."))
    (process-close p)
  )

  ;; 2. JSON 模式：tool_execution_start/end 事件流与参数提取
  (let* ((cmd (list GF_BIN "pi" "--mode" "json" "--model" "agent-web-search"
                "--tools" "+web_search" "Search goldfish scheme"
              )
         )
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (raw-out (read-all p))
         (code (process-wait p 15000))
         (lines (split-json-lines raw-out))
         (events (map string->json lines))
         (event-types (map (lambda (ev) (cdr (assoc "type" ev))) events))
        )
    (check code => 0)
    ;; 验证完整事件序列
    (check (car event-types) => "session")
    (check (car (reverse event-types)) => "agent_settled")
    (check-true (pair? (member "tool_execution_start" event-types)))
    (check-true (pair? (member "tool_execution_end" event-types)))

    ;; 找到 tool_execution_start 事件，验证 toolName 与 query 参数
    (let* ((start-ev (find (lambda (ev) (equal? (cdr (assoc "type" ev)) "tool_execution_start"))
                           events))
           (t-name (cdr (assoc "toolName" start-ev)))
           (args (cdr (assoc "args" start-ev)))
           (query (cdr (assoc "query" args))))
      (check t-name => "web_search")
      (check query => "Goldfish Scheme"))

    ;; 找到 tool_execution_end 事件，验证结果包含 details 与 sources
    (let* ((end-ev (find (lambda (ev) (equal? (cdr (assoc "type" ev)) "tool_execution_end"))
                         events))
           (t-name (cdr (assoc "toolName" end-ev)))
           (is-err (cdr (assoc "isError" end-ev)))
           (res (cdr (assoc "result" end-ev))))
      (check t-name => "web_search")
      (check-true (or (eq? is-err #f) (eq? is-err 'false)))
      (check-true (pair? (assoc "sources" res)))
      (check-true (pair? (assoc "details" res))))
    (process-close p)
  )

  ;; 3. 混合工具调用保障：既有 bash 工具正常工作
  (let* ((cmd (list GF_BIN "pi" "--model" "multi-turn" "--tools" "bash,web_search"
                "--print" "Run multi turn"
              )
         )
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 15000))
         (out (process-read p :stream 'stdout))
        )
    (check code => 0)
    (check-true (string-contains? out "Multi-turn tool completed successfully!"))
    (process-close p)
  )

        ) ;lambda
        (lambda ()
          (process-kill proc)
          (process-close proc)
        ) ;lambda
      ) ;dynamic-wind
    ) ;let*
  ) ;lambda
  (lambda ()
    (cleanup-fixtures!)
  ) ;lambda
) ;dynamic-wind

(check-report)
