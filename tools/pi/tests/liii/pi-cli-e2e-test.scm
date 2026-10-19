(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme process-context) (liii check) (liii string) (liii path) (liii subprocess) (liii pi))

(check-set-mode! 'report-failed)

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define stub-server-file
  (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
    "tools/pi/tests/stub_gemini_server.py"
    (if (file-exists? "tests/stub_gemini_server.py")
      "tests/stub_gemini_server.py"
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
                   (cons "HTTP_PROXY" "")
                 ) ;list
       ) ;test-env
      ) ;
  (check-true (number? port))

  ;; 1. print 模式正常应答（退出码 0，stdout 有输出）
  (let* ((cmd (list GF_BIN "pi" "--print" "Hello from e2e test"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
        ) ;
    (check code => 0)
    (check-true (or (string-contains? out "Hello from stub!")
                  (string-contains? out "Hello stream!")
                ) ;or
    ) ;check-true
    (process-close p)
  ) ;let*

  ;; 2. 多轮 functionCall 端到端（退出码 0，stdout 有输出）
  (let* ((cmd (list GF_BIN "pi" "--model" "multi-turn" "--print" "Run multi turn steps"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 15000))
         (out (process-read p :stream 'stdout))
        ) ;
    (check code => 0)
    (check-true (string-contains? out "Multi-turn tool completed successfully!"))
    (process-close p)
  ) ;let*

  ;; 3. 截断 MAX_TOKENS（退出码 1，stdout 部分输出，stderr 提示截断）
  (let* ((cmd (list GF_BIN "pi" "--model" "max-tokens" "--print" "Run max tokens"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
         (err (process-read p :stream 'stderr))
        ) ;
    (check code => 1)
    (check-true (string-contains? out "Partial cut"))
    (check-true (string-contains? err
                  "Response was truncated: the model reached its output token limit."
                ) ;string-contains?
    ) ;check-true
    (process-close p)
  ) ;let*

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
    (process-close p)
  ) ;let*

  ;; 5. 轮数超限 max-turns（退出码 1，stderr 提示 Maximum turns）
  (let* ((cmd (list GF_BIN "pi" "--model" "multi-turn" "--max-turns" "1"
                "--print" "Run max turns"
              ) ;list
         ) ;cmd
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (err (process-read p :stream 'stderr))
        ) ;
    (check code => 1)
    (check-true (string-contains? err "Maximum turns limit reached"))
    (process-close p)
  ) ;let*

  ;; 6. 项目上下文 AGENTS.md / CLAUDE.md 注入验证
  (let* ((cmd (list GF_BIN "pi" "--model" "check-project-context" "--print" "Check context")
         ) ;cmd
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
        ) ;
    (check code => 0)
    (check-true (string-contains? out "Project context verified: CLAUDE.md is present")
    ) ;check-true
    (process-close p)
  ) ;let*

  ;; 7. CLI --provider anthropic print 模式端到端测试
  ;; GF_PI_MODEL 显式指定模型，避免与 provider 不匹配（任务 034 严格校验）
  (let* ((ant-env (append test-env
                           (list (cons "ANTHROPIC_API_KEY" "sk-ant-test-e2e")
                             (cons "ANTHROPIC_BASE_URL" base-url)
                             (cons "GF_PI_MODEL" "claude-sonnet-4-5"))))
         (cmd (list GF_BIN "pi" "--provider" "anthropic" "--print" "Hello Anthropic CLI")
         ) ;cmd
         (p (process-spawn cmd :env ant-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
        ) ;
    (check code => 0)
    (check-true (or (string-contains? out "Hello from Claude stub!")
                  (string-contains? out "from Claude stream!")
                ) ;or
    ) ;check-true
    (process-close p)
  ) ;let*

  ;; 8. 错误路径退出码测试：模型错误时退出码必须为 1（§8 / 缺陷7）
  (let* ((ant-env
           (append (list (cons "ANTHROPIC_API_KEY" "sk-ant-test-e2e")
                     (cons "ANTHROPIC_BASE_URL" (string-append base-url "/error-400"))
                     (cons "GF_PI_BASE_URL" (string-append base-url "/error-400"))
                   ) ;list
             test-env
           ) ;append
         ) ;ant-env
         (cmd (list GF_BIN "pi" "--provider" "anthropic" "--model" "fake-model"
                "--print" "Hello Anthropic error"
              ) ;list
         ) ;cmd
         (p (process-spawn cmd :env ant-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
         (err (process-read p :stream 'stderr))
        ) ;
    (check code => 1)
    (check-true (string-contains? err "HTTP 400"))
    (process-close p)
  ) ;let*

  ;; 9. 缺陷8防回归测试：4字节 UTF-8 字符（emoji）与 '<' 组合在 stub server 下端到端验证
  (let* ((cli-env (append (list (cons "GF_PI_BASE_URL" base-url)) test-env))
         (cmd (list GF_BIN "pi" "--provider" "google" "--model"
                "gemini-3.8-flash" "--print" "🚀 <"
              ) ;list
         ) ;cmd
         (p (process-spawn cmd :env cli-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout))
        ) ;
    (check code => 0)
    (check-true (or (string-contains? out "Hello from stub!") (string-contains? out "stream!"))
    ) ;check-true
    (process-close p)
  ) ;let*

  ;; 10. CLI --provider deepseek 凭据缺失时明确报错退出码 1
  (let* ((no-key-env (list (cons "PI_CODING_AGENT_DIR" "non_existent_dir_xyz")
                           (cons "HOME" "/non-existent")
                           (cons "PATH" (or (get-environment-variable "PATH") ""))))
         (cmd (list GF_BIN "pi" "--provider" "deepseek" "--print" "Hello missing key"))
         (p (process-spawn cmd :env no-key-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (err (process-read p :stream 'stderr)))
    (check code => 1)
    (check-true (string-contains? err "No credentials found for provider \"deepseek\""))
    (process-close p))

  ;; 11. CLI --provider deepseek print 模式端到端测试（通过 stub server）
  ;; GF_PI_MODEL 显式指定模型，避免与 provider 不匹配（任务 034 严格校验）
  (let* ((ds-env (append test-env
                           (list (cons "DEEPSEEK_API_KEY" "ds-test-key-e2e")
                             (cons "DEEPSEEK_BASE_URL" base-url)
                             (cons "ANTHROPIC_BASE_URL" base-url)
                             (cons "GF_PI_MODEL" "deepseek-v4-pro")
                             (cons "GF_PI_BASE_URL" base-url))))
         (cmd (list GF_BIN "pi" "--provider" "deepseek" "--print" "Hello DeepSeek CLI"))
         (p (process-spawn cmd :env ds-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (out (process-read p :stream 'stdout)))
    (check code => 0)
    (check-true (or (string-contains? out "Hello from Claude stub!")
                    (string-contains? out "from Claude stream!")))
    (process-close p))

  ;; 12. CLI --session-timeout 0 关闭兜底端到端测试
  (let* ((cmd (list GF_BIN "pi" "--model" "multi-turn" "--session-timeout" "0" "--print" "Run multi turn"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 15000))
         (out (process-read p :stream 'stdout)))
    (check code => 0)
    (check-true (string-contains? out "Multi-turn tool completed successfully!"))
    (process-close p))

  ;; 13. CLI --session-timeout 立即超时（负数）端到端测试
  (let* ((cmd (list GF_BIN "pi" "--model" "multi-turn" "--session-timeout" "-1" "--print" "Run timeout"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (err (process-read p :stream 'stderr)))
    (check code => 1)
    (check-true (string-contains? err "Session timeout exceeded"))
    (process-close p))

  (process-kill proc)
) ;let*

(check-report)
