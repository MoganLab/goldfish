(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define stub-server-file
  (if (file-exists? "tests/stub_gemini_server.py")
    "tests/stub_gemini_server.py"
    (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
      "tools/pi/tests/stub_gemini_server.py"
      "../../tools/pi/tests/stub_gemini_server.py"
    ) ;if
  ) ;if
) ;define

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/direct")
    "tests/fixtures/direct"
    (if (file-exists? "tools/pi/tests/fixtures/direct")
      "tools/pi/tests/fixtures/direct"
      "../../tools/pi/tests/fixtures/direct"
    ) ;if
  ) ;if
) ;define

(define GF_BIN
  (if (file-exists? "bin/gf")
    "bin/gf"
    (if (file-exists? "../../bin/gf") "../../bin/gf" "bin/gf")
  ) ;if
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
       (test-env (list (cons "PI_CODING_AGENT_DIR" FIXTURE_DIR)
                       (cons "GF_PI_BASE_URL" base-url)
                       (cons "GF_PI_MODEL" "gemini-3.8-flash")
                       (cons "https_proxy" "")
                       (cons "http_proxy" "")
                       (cons "all_proxy" "")
                       (cons "ALL_PROXY" "")
                       (cons "HTTPS_PROXY" "")
                       (cons "HTTP_PROXY" "")))
      ) ;
  (check-true (number? port))

  ;; 1. print 模式正常应答（退出码 0，stdout 有输出）
  (let* ((cmd (list GF_BIN "pi" "--print" "Hello from e2e test"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
        ) ;
    (check code => 0)
    (check-true (string-contains? out "Hello from stub!"))
    (process-close p))

  ;; 2. 多轮 functionCall 端到端（退出码 0，stdout 有输出）
  (let* ((cmd (list GF_BIN "pi" "--model" "multi-turn" "--print" "Run multi turn steps"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 15000))
         (out (process-read p :stream 'stdout))
        ) ;
    (check code => 0)
    (check-true (string-contains? out "Multi-turn tool completed successfully!"))
    (process-close p))

  ;; 3. 截断 MAX_TOKENS（退出码 1，stdout 部分输出，stderr 提示截断）
  (let* ((cmd (list GF_BIN "pi" "--model" "max-tokens" "--print" "Run max tokens"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
         (err (process-read p :stream 'stderr))
        ) ;
    (check code => 1)
    (check-true (string-contains? out "Partial cut"))
    (check-true (string-contains? err "Response was truncated: the model reached its output token limit."))
    (process-close p))

  ;; 4. 报错 SAFETY（退出码 1，stdout 无输出，stderr 提示错误）
  (let* ((cmd (list GF_BIN "pi" "--model" "safety" "--print" "Run safety check"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
         (err (process-read p :stream 'stderr))
        ) ;
    (check code => 1)
    (check-true (or (eof-object? out) (string-null? (string-trim-both out))))
    (check-true (string-contains? err "SAFETY"))
    (process-close p))

  ;; 5. 轮数超限 max-turns（退出码 1，stderr 提示 Maximum turns）
  (let* ((cmd (list GF_BIN "pi" "--model" "multi-turn" "--max-turns" "1" "--print" "Run max turns"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (err (process-read p :stream 'stderr))
        ) ;
    (check code => 1)
    (check-true (string-contains? err "Maximum turns limit reached"))
    (process-close p))

  ;; 6. 项目上下文 AGENTS.md / CLAUDE.md 注入验证
  (let* ((cmd (list GF_BIN "pi" "--model" "check-project-context" "--print" "Check context"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
        ) ;
    (check code => 0)
    (check-true (string-contains? out "Project context verified: CLAUDE.md is present"))
    (process-close p))

  (process-kill proc)
) ;let*

(check-report)
