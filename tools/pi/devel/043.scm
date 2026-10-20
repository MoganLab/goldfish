(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii json)
  (liii path)
  (liii string)
  (liii subprocess)
  (liii pi)
  (liii pi tools)
  (liii pi message)
  (liii pi agent tools)
  (liii pi gemini)
  (liii pi web-search)
) ;import

(check-set-mode! 'report-failed)

;; [043] 验收脚本：web_search Agent 工具调用闭环联调
;; 逐条对应 tools/pi/devel/043.md 验收点 1-5

(define %tmp-files '())

(define (%capture tag cmd timeout)
  (let* ((tmp (string-append "tmp-043-" tag ".txt")))
    (set! %tmp-files (cons tmp %tmp-files))
    (let* ((ret (run (list (pi-find-bash-path) "-c" (string-append "(" cmd ") > " tmp " 2>&1"))
                  :stdin   'null
                  :timeout timeout
                ) ;run
           ) ;ret
           (out
             (if (and (file-exists? tmp) (path-file? (path tmp))) (path-read-text tmp) "")
           ) ;out
          ) ;
      (cons ret out)
    ) ;let*
  ) ;let*
) ;define

(define (%ok? tag cmd timeout)
  (let* ((res (%capture tag cmd timeout)) (rc (car res)))
    (unless (zero? rc)
      (display (string-append "!!! " tag " exited " (number->string rc) "\n" (cdr res))
      ) ;display
    ) ;unless
    rc
  ) ;let*
) ;define

(define (%cleanup!)
  (for-each (lambda (f) (when (file-exists? f) (delete-file f))) %tmp-files)
  (set! %tmp-files '())
) ;define

(define (find-stub-server)
  (let loop
    ((c '("tools/pi/tests/stub_gemini_server.py" "tests/stub_gemini_server.py"
          "stub_gemini_server.py" "../stub_gemini_server.py"
          "../../stub_gemini_server.py")
     ) ;c
    ) ;
    (cond ((null? c) (error 'file-not-found "stub_gemini_server.py not found"))
          ((file-exists? (car c)) (car c))
          (else (loop (cdr c)))
    ) ;cond
  ) ;let
) ;define

(define (parse-port line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (display "=== 验收点 1: Gemini 模型注入 web_search 工具声明 ===\n")
    (let* ((contents #((("role" . "user") ("parts" . #((("text" . "search")))))))
           (decls (gemini-build-tool-declarations '("web_search")))
           (d0 (car decls))
           (body (gemini-build-request-body contents #f decls))
           (tools (cdr (assoc "tools" body)))
           (tc (cdr (assoc "toolConfig" body)))
          ) ;
      (check-true (pair? decls))
      (check (cdr (assoc "name" d0)) => "web_search")
      (check-true (string-contains? (cdr (assoc "description" d0)) "Google Search"))
      (check-true (pair? (assoc "parametersJsonSchema" d0)))
      (check (vector-length tools) => 1)
      (let ((fns (cdr (assoc "functionDeclarations" (vector-ref tools 0)))))
        (check (vector-length fns) => 1)
        (check (cdr (assoc "name" (vector-ref fns 0))) => "web_search")
      ) ;let
      (check
        (cdr (assoc "mode" (cdr (assoc "functionCallingConfig" tc))))
        =>
        "AUTO"
      ) ;check
      ;; 兼容 Grounding tools vector 结构
      (let* ((tools-vec
               (vector (list (cons "googleSearch" '(()))))
             ) ;tools-vec
             (body-g (gemini-build-request-body contents #f tools-vec))
            ) ;
        (check (vector-length (cdr (assoc "tools" body-g))) => 1)
        (check-true (pair? (assoc "toolConfig" body-g)))
      ) ;let*
      ;; 无 tools 时不注入
      (let ((empty-body (gemini-build-request-body contents #f '())))
        (check (assoc "tools" empty-body) => #f)
        (check (assoc "toolConfig" empty-body) => #f)
      ) ;let
    ) ;let*

    (display "=== 验收点 2: Agent 事件循环调度执行器与回传 tool_result 闭环 ===\n"
    ) ;display
    (let* ((call
             (list (cons 'id "c1")
               (cons 'name "bash")
               (cons 'arguments (list (cons "command" "echo exec-tst")))
             ) ;list
           ) ;call
           (msg (pi-execute-one-tool-call call #f 10))
          ) ;
      (check-true (pi-tool-result-message? msg))
      (check (pi-tool-result-message-tool-call-id msg) => "c1")
      (check (pi-tool-result-message-tool-name msg) => "bash")
      (check (pi-text-block-text (car (pi-tool-result-message-content msg)))
        =>
        "exec-tst\n"
      ) ;check
    ) ;let*
    (let* ((calls
             (list
               (list (cons 'id "c2")
                 (cons 'name "bash")
                 (cons 'arguments (list (cons "command" "echo ok-cfg")))
               ) ;list
             ) ;list
           ) ;calls
           (res (pi-execute-tool-calls calls #f 10 '((provider . "google"))))
          ) ;
      (check (length res) => 1)
      (check
        (pi-text-block-text (car (pi-tool-result-message-content (car res))))
        =>
        "ok-cfg\n"
      ) ;check
    ) ;let*

    (display "=== 验收点 3 & 4: CLI 前端展示、端到端闭环与既有工具回归 ===\n"
    ) ;display
    (let* ((stub-py (find-stub-server))
           (proc (process-spawn (list "python3" stub-py) :stdout 'pipe))
           (port (parse-port (process-read-line proc)))
           (base-url (string-append "http://127.0.0.1:" (number->string port)))
           (test-env (list (cons "PI_CODING_AGENT_DIR" "tools/pi/tests/fixtures/direct")
                       (cons "GF_PI_BASE_URL" base-url)
                       (cons "GOOGLE_BASE_URL" base-url)
                       (cons "GF_PI_MODEL" "agent-web-search")
                       (cons "GOOGLE_SEARCH_MODEL" "web-search-basic")
                       (cons "GOOGLE_API_KEY" "test-key-search")
                       (cons "https_proxy" "")
                       (cons "http_proxy" "")
                       (cons "all_proxy" "")
                       (cons "ALL_PROXY" "")
                     ) ;list
           ) ;test-env
          ) ;
      (check-true (number? port))

      ;; 3a. print 模式：闭环搜索并呈现 query 与 Sources 引用
      (let* ((cmd (list "bin/gf" "pi" "--model" "agent-web-search" "--tools"
                    "+web_search" "--print" "Search goldfish scheme"
                  ) ;list
             ) ;cmd
             (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
             (code (process-wait p 15000))
             (out (process-read p :stream 'stdout))
             (err (process-read p :stream 'stderr))
            ) ;
        (check code => 0)
        (check-true (string-contains? out "Goldfish Scheme is a minimalist Scheme implementation")
        ) ;check-true
        (check-true (string-contains? out "## Sources"))
        (check-true (string-contains? out "https://goldfish.liii.pro"))
        (check-true (string-contains? err "Searching web: Goldfish Scheme..."))
        (process-close p)
      ) ;let*

      ;; 3b. JSON 模式：事件流完整性与 tool_execution 事件
      (let* ((cmd (list "bin/gf" "pi" "--mode" "json" "--model"
                    "agent-web-search" "--tools" "+web_search"
                    "Search goldfish scheme"
                  ) ;list
             ) ;cmd
             (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
             (code (process-wait p 15000))
             (raw-out (process-read p :stream 'stdout))
             (lines
               (filter (lambda (s) (not (string-null? s))) (string-split raw-out "\n"))
             ) ;lines
             (events (map string->json lines))
             (types
               (map (lambda (ev) (cdr (assoc "type" ev))) events)
             ) ;types
            ) ;
        (check code => 0)
        (check (car types) => "session")
        (check (car (reverse types)) => "agent_settled")
        (check-true (pair? (member "tool_execution_start" types)))
        (check-true (pair? (member "tool_execution_end" types)))

        (let* ((start-ev
                 (find
                   (lambda (e) (equal? (cdr (assoc "type" e)) "tool_execution_start"))
                   events
                 ) ;find
               ) ;start-ev
               (end-ev
                 (find
                   (lambda (e) (equal? (cdr (assoc "type" e)) "tool_execution_end"))
                   events
                 ) ;find
               ) ;end-ev
              ) ;
          (check (cdr (assoc "toolName" start-ev)) => "web_search")
          (check
            (cdr (assoc "query" (cdr (assoc "args" start-ev))))
            =>
            "Goldfish Scheme"
          ) ;check
          (check (cdr (assoc "toolName" end-ev)) => "web_search")
          (check-true
            (pair? (assoc "sources" (cdr (assoc "result" end-ev))))
          ) ;check-true
          (check-true
            (pair? (assoc "details" (cdr (assoc "result" end-ev))))
          ) ;check-true
        ) ;let*
        (process-close p)
      ) ;let*

      ;; 4a. 混合工具保障：multi-turn 模型既有工具调用
      (let* ((cmd (list "bin/gf" "pi" "--model" "multi-turn" "--tools"
                    "bash,web_search" "--print" "Run multi turn"
                  ) ;list
             ) ;cmd
             (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
             (code (process-wait p 15000))
             (out (process-read p :stream 'stdout))
            ) ;
        (check code => 0)
        (check-true (string-contains? out "Multi-turn tool completed successfully!"))
        (process-close p)
      ) ;let*

      (process-kill proc)
      (process-wait proc)
    ) ;let*

    (display "=== 验收点 5: 门禁与工程规范 ===\n")
    (check (%ok? "build" "timeout 280 xmake b goldfish" 300) => 0)
    (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
    (check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
    (check (%ok? "doc" "timeout 120 bin/gf doc --build-json" 150) => 0)
    (check (%ok? "lines"
             "for f in tools/pi/liii/pi/gemini/core.scm tools/pi/liii/pi/agent.scm tools/pi/liii/pi/agent/tools.scm tools/pi/liii/pi/web-search/config.scm; do [ $(wc -l < \"$f\") -le 300 ] || exit 1; done"
             30
           ) ;%ok?
      =>
      0
    ) ;check

    ;; 既有与新增测试全绿
    (for-each
      (lambda (file)
        (check (%ok? "test" (string-append "timeout 120 bin/gf test " file) 150) => 0)
      ) ;lambda
      '("tools/pi/tests/liii/pi/gemini-build-request-body-test.scm"
        "tools/pi/tests/liii/pi/agent/tools/pi-execute-one-tool-call-test.scm"
        "tools/pi/tests/liii/pi/agent/tools/pi-execute-tool-calls-test.scm"
        "tools/pi/tests/liii/pi/web-search/pi-web-search-agent-loop-test.scm"
        "tools/pi/tests/liii/pi-cli-e2e-test.scm"
        "tools/pi/tests/liii/pi-json-e2e-test.scm")
    ) ;for-each
  ) ;lambda
  (lambda () (%cleanup!) (check-report))
) ;dynamic-wind
