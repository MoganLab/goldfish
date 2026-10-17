(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii subprocess)
  (liii pi)
  (liii goldpi-agent)
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

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (check-true (number? port))

  ;; 1. 单轮正常应答
  (let* ((cfg (list (cons 'provider "google")
                    (cons 'api-key "test-stub-key")
                    (cons 'model-id "gemini-3.8-flash")
                    (cons 'model-meta (list (cons 'base-url base-url)))
                    (cons 'proxy-url #f)
                    (cons 'tools '("read" "bash"))
                    (cons 'settings '())
                    (cons 'timeout 10)
                    (cons 'max-turns 10)))
         (init-msgs (list '((role . "user") (content . "Hello"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check (extract-assistant-text last-msg) => "Hello from stub!"))

  ;; 2. 多轮 functionCall 端到端（3 轮闭环）
  (let* ((cfg (list (cons 'provider "google")
                    (cons 'api-key "test-stub-key")
                    (cons 'model-id "multi-turn")
                    (cons 'model-meta (list (cons 'base-url base-url)))
                    (cons 'proxy-url #f)
                    (cons 'tools '("read" "bash"))
                    (cons 'settings '())
                    (cons 'timeout 10)
                    (cons 'max-turns 10)))
         (init-msgs (list '((role . "user") (content . "Run multi steps"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check (extract-assistant-text last-msg) => "Multi-turn tool completed successfully!")
    ;; 验证消息链条：user -> assistant(toolUse) -> toolResult -> assistant(toolUse) -> toolResult -> assistant(stop)
    (check (length res-msgs) => 6))

  ;; 3. MAX_TOKENS 截断
  (let* ((cfg (list (cons 'provider "google")
                    (cons 'api-key "test-stub-key")
                    (cons 'model-id "max-tokens")
                    (cons 'model-meta (list (cons 'base-url base-url)))
                    (cons 'proxy-url #f)
                    (cons 'tools '("read" "bash"))
                    (cons 'settings '())
                    (cons 'timeout 10)
                    (cons 'max-turns 10)))
         (init-msgs (list '((role . "user") (content . "Long text"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'stopReason last-msg)) => "length")
    (check (extract-assistant-text last-msg) => "Partial cut"))

  ;; 4. SAFETY 报错
  (let* ((cfg (list (cons 'provider "google")
                    (cons 'api-key "test-stub-key")
                    (cons 'model-id "safety")
                    (cons 'model-meta (list (cons 'base-url base-url)))
                    (cons 'proxy-url #f)
                    (cons 'tools '("read" "bash"))
                    (cons 'settings '())
                    (cons 'timeout 10)
                    (cons 'max-turns 10)))
         (init-msgs (list '((role . "user") (content . "Unsafe text"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'stopReason last-msg)) => "error")
    (check (cdr (assoc 'errorMessage last-msg)) => "SAFETY"))

  ;; 5. max-turns 超限
  (let* ((cfg (list (cons 'provider "google")
                    (cons 'api-key "test-stub-key")
                    (cons 'model-id "multi-turn")
                    (cons 'model-meta (list (cons 'base-url base-url)))
                    (cons 'proxy-url #f)
                    (cons 'tools '("read" "bash"))
                    (cons 'settings '())
                    (cons 'timeout 10)
                    (cons 'max-turns 1)))
         (init-msgs (list '((role . "user") (content . "Run multi steps"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 1))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'stopReason last-msg)) => "error")
    (check-true (string-contains? (cdr (assoc 'errorMessage last-msg)) "Maximum turns")))

  (process-kill proc)
) ;let*

(check-report)
