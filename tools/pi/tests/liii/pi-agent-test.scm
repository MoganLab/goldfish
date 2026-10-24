(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii list)
  (liii subprocess)
  (liii pi agent) (liii pi ai message) (liii pi session)
  (liii pi agent)
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

(define (make-test-cfg provider api-key model-id base-url max-turns . tools-opt)
  (let ((tools (if (pair? tools-opt) (car tools-opt) '("read" "bash"))))
    (list (cons 'provider provider)
          (cons 'api-key api-key)
          (cons 'model-id model-id)
          (cons 'model-meta (list (cons 'base-url base-url) (cons 'max-tokens 4096)))
          (cons 'proxy-url #f)
          (cons 'tools tools)
          (cons 'settings '())
          (cons 'timeout 10)
          (cons 'max-turns max-turns))))

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (check-true (number? port))

  ;; 1. 单轮正常应答
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "gemini-3.8-flash" base-url 10))
         (init-msgs (list '((role . "user") (content . "Hello"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check (extract-assistant-text last-msg) => "Hello stream!"))

  ;; 2. 多轮 functionCall 端到端（3 轮闭环）
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "multi-turn" base-url 10))
         (init-msgs (list '((role . "user") (content . "Run multi steps"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check (extract-assistant-text last-msg) => "Multi-turn tool completed successfully!")
    (check (length res-msgs) => 6))

  ;; 3. MAX_TOKENS 截断
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "max-tokens" base-url 10))
         (init-msgs (list '((role . "user") (content . "Long text"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'stopReason last-msg)) => "length")
    (check (extract-assistant-text last-msg) => "Partial cut"))

  ;; 4. SAFETY 报错
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "safety" base-url 10))
         (init-msgs (list '((role . "user") (content . "Unsafe text"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'stopReason last-msg)) => "error")
    (check (cdr (assoc 'errorMessage last-msg)) => "SAFETY"))

  ;; 5. max-turns 超限
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "multi-turn" base-url 1))
         (init-msgs (list '((role . "user") (content . "Run multi steps"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 1))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'stopReason last-msg)) => "error")
    (check-true (string-contains? (cdr (assoc 'errorMessage last-msg)) "Maximum turns")))

  ;; 6. 流式事件发射验证（message_update 与 text_delta）
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "gemini-3.8-flash" base-url 10))
         (init-msgs (list '((role . "user") (content . "Hello stream"))))
         (emitted '())
         (emit (lambda (ev-type . args)
                 (set! emitted (cons (cons ev-type args) emitted))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" emit 10 10)))
    (check-true (pair? res-msgs))
    (let ((updates (filter (lambda (ev) (eq? (car ev) 'message_update)) emitted)))
      (check-true (> (length updates) 0))
      (let* ((upd (car (reverse updates)))
             (usage (cadr upd))
             (a-ev (caddr upd)))
        (check-true (pair? usage))
        (check-true (if (member (cdr (assoc 'type a-ev)) '("text_start" "text_delta")) #t #f)))))

  ;; 7. Anthropic provider 单轮应答与流式事件发射
  (let* ((cfg (make-test-cfg "anthropic" "sk-ant-test" "claude-3-7-sonnet-20250219"
                             (string-append base-url "/stream") 10))
         (init-msgs (list '((role . "user") (content . "Hello anthropic"))))
         (emitted '())
         (emit (lambda (ev-type . args)
                 (set! emitted (cons (cons ev-type args) emitted))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" emit 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (let ((updates (filter (lambda (ev) (eq? (car ev) 'message_update)) emitted)))
      (check-true (> (length updates) 0))))

  ;; 8. Anthropic provider 多轮工具调用端到端
  (let* ((cfg (make-test-cfg "anthropic" "sk-ant-test" "claude-3-7-sonnet-20250219"
                             (string-append base-url "/tool-call") 10 '("bash")))
         (init-msgs (list '((role . "user") (content . "Run echo test"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check-true (string-contains? (extract-assistant-text last-msg) "Tool executed successfully!"))
    (let ((tool-results (filter (lambda (m) (equal? (cdr (assoc 'role m)) "toolResult")) res-msgs)))
      (check (= (length tool-results) 1) => #t)))

  ;; 9. GLM provider 单轮应答与流式事件发射（复用 Anthropic 链路）
  (let* ((cfg (make-test-cfg "glm" "glm-token-test" "glm-4-plus"
                             (string-append base-url "/stream") 10))
         (init-msgs (list '((role . "user") (content . "Hello glm"))))
         (emitted '())
         (emit (lambda (ev-type . args)
                 (set! emitted (cons (cons ev-type args) emitted))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" emit 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (let ((updates (filter (lambda (ev) (eq? (car ev) 'message_update)) emitted)))
      (check-true (> (length updates) 0))))

  ;; 10. GLM provider 多轮工具调用端到端
  (let* ((cfg (make-test-cfg "glm" "glm-token-test" "glm-4-plus"
                             (string-append base-url "/tool-call") 10 '("bash")))
         (init-msgs (list '((role . "user") (content . "Run echo glm test"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check-true (string-contains? (extract-assistant-text last-msg) "Tool executed successfully!"))
    (let ((tool-results (filter (lambda (m) (equal? (cdr (assoc 'role m)) "toolResult")) res-msgs)))
      (check (= (length tool-results) 1) => #t)))

  ;; 11. DeepSeek provider 单轮应答与流式事件发射
  (let* ((cfg (make-test-cfg "deepseek" "ds-token-test" "deepseek-chat"
                             (string-append base-url "/stream") 10))
         (init-msgs (list '((role . "user") (content . "Hello deepseek"))))
         (emitted '())
         (emit (lambda (ev-type . args)
                 (set! emitted (cons (cons ev-type args) emitted))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" emit 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (let ((updates (filter (lambda (ev) (eq? (car ev) 'message_update)) emitted)))
      (check-true (> (length updates) 0))))

  ;; 12. DeepSeek provider 多轮工具调用端到端
  (let* ((cfg (make-test-cfg "deepseek" "ds-token-test" "deepseek-chat"
                             (string-append base-url "/tool-call") 10 '("bash")))
         (init-msgs (list '((role . "user") (content . "Run echo deepseek test"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check-true (string-contains? (extract-assistant-text last-msg) "Tool executed successfully!"))
    (let ((tool-results (filter (lambda (m) (equal? (cdr (assoc 'role m)) "toolResult")) res-msgs)))
      (check (= (length tool-results) 1) => #t)))

  ;; 13. session-timeout 兜底：多轮循环累计超时结构化退出
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "multi-turn" base-url 10))
         (init-msgs (list '((role . "user") (content . "Run multi steps"))))
         ;; 传入 session-timeout = -1，迫使在第 1 轮 toolUse 之后立刻超时
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10 -1))
         (last-msg (car (reverse res-msgs))))
    (check (pi-assistant-message-stop-reason last-msg) => "error")
    (check-true (string-contains? (pi-assistant-message-error-message last-msg) "Session timeout exceeded")))

  ;; 14. session-timeout = 0 关闭兜底：多轮循环正常完成
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "multi-turn" base-url 10))
         (init-msgs (list '((role . "user") (content . "Run multi steps"))))
         ;; 传入 session-timeout = 0 关闭超时兜底
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10 0))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check (extract-assistant-text last-msg) => "Multi-turn tool completed successfully!")
    (check (length res-msgs) => 6))

  ;; 15. session-timeout > 0 正数且未超时：正常完成
  (let* ((cfg (make-test-cfg "google" "test-stub-key" "multi-turn" base-url 10))
         (init-msgs (list '((role . "user") (content . "Run multi steps"))))
         (res-msgs (run-agent-loop cfg init-msgs "SysPrompt" #f 10 10 60))
         (last-msg (car (reverse res-msgs))))
    (check (cdr (assoc 'role last-msg)) => "assistant")
    (check (cdr (assoc 'stopReason last-msg)) => "stop")
    (check (extract-assistant-text last-msg) => "Multi-turn tool completed successfully!"))

  (process-kill proc)
) ;let*

(check-report)
