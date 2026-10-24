(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (liii check) (liii string) (liii path) (liii json)
        (liii subprocess) (liii pi ai event) (liii pi ai gemini) (liii pi code config) (liii pi ai gemini))

(check-set-mode! 'report-failed)

;; 1. gemini-is-retriable-error?
(check (gemini-is-retriable-error? 200 "{}") => #f)
(check (gemini-is-retriable-error? 400 "{}") => #f)
(check (gemini-is-retriable-error? 408 "{}") => #t)
(check (gemini-is-retriable-error? 409 "{}") => #t)
(check (gemini-is-retriable-error? 429 "{}") => #t)
(check (gemini-is-retriable-error? 500 "{}") => #t)
(check (gemini-is-retriable-error? 503 "{}") => #t)
(check (gemini-is-retriable-error? 0 "") => #t)

;; 2. gemini-parse-response (纯函数单测)
;; 正常普通回复
(let* ((resp-json '(("candidates" . #((("content" . (("parts" . #((("text" . "Hello world"))))))
                                      ("finishReason" . "STOP"))))
                    ("usageMetadata" . (("promptTokenCount" . 10)
                                        ("candidatesTokenCount" . 5)
                                        ("totalTokenCount" . 15)))))
       (parsed (gemini-parse-response resp-json 200)))
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 1)
    (check (cdr (assoc 'text (car content))) => "Hello world"))
  (let ((u (cdr (assoc 'usage parsed))))
    (check (cdr (assoc 'input u)) => 10)
    (check (cdr (assoc 'output u)) => 5)))

;; functionCall 回复
(let* ((resp-json '(("candidates" . #((("content" . (("parts" . #((("functionCall" . (("name" . "bash")
                                                                                       ("args" . (("command" . "ls")))
                                                                                       ("id" . "call_123"))))))))
                                      ("finishReason" . "STOP"))))))
       (parsed (gemini-parse-response resp-json 200)))
  (check (cdr (assoc 'stopReason parsed)) => "toolUse")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 1)
    (let ((blk (car content)))
      (check (cdr (assoc 'type blk)) => "toolCall")
      (check (cdr (assoc 'id blk)) => "call_123")
      (check (cdr (assoc 'name blk)) => "bash")
      (check (cdr (assoc "command" (cdr (assoc 'arguments blk)))) => "ls"))))

;; MAX_TOKENS 截断
(let* ((resp-json '(("candidates" . #((("content" . (("parts" . #((("text" . "partial"))))))
                                      ("finishReason" . "MAX_TOKENS"))))))
       (parsed (gemini-parse-response resp-json 200)))
  (check (cdr (assoc 'stopReason parsed)) => "length"))

;; SAFETY 报错
(let* ((resp-json '(("candidates" . #((("content" . (("parts" . #())))
                                      ("finishReason" . "SAFETY"))))))
       (parsed (gemini-parse-response resp-json 200)))
  (check (cdr (assoc 'stopReason parsed)) => "error")
  (check (cdr (assoc 'errorMessage parsed)) => "SAFETY"))

;; 带有 thinking part 的响应解析
(let* ((resp-json '(("candidates" . #((("content" . (("parts" . #((("text" . "Let me ponder...")
                                                                   ("thought" . #t)
                                                                   ("thoughtSignature" . "sig-abc"))
                                                                  (("text" . "The final answer."))))))
                                      ("finishReason" . "STOP"))))))
       (parsed (gemini-parse-response resp-json 200)))
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 2)
    (let ((b1 (car content))
          (b2 (cadr content)))
      (check (cdr (assoc 'type b1)) => "thinking")
      (check (cdr (assoc 'thinking b1)) => "Let me ponder...")
      (check (cdr (assoc 'thinkingSignature b1)) => "sig-abc")
      (check (cdr (assoc 'type b2)) => "text")
      (check (cdr (assoc 'text b2)) => "The final answer."))))

;; 3. 端到端测试：本地 stub server
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

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (check-true (number? port))

  (let* ((cfg (list (cons 'provider "google")
                    (cons 'api-key "test-key-stub-123")
                    (cons 'model-id "gemini-3.8-flash")
                    (cons 'model-meta (list (cons 'base-url base-url)))
                    (cons 'proxy-url #f)
                    (cons 'tools '("read" "bash"))
                    (cons 'settings '())
              ))
         ;; 3.1 正常调用
         (resp1 (gemini-request cfg (list '((role . "user") (content . "Hello"))) "System prompt" '("bash") 10))
        ) ;
    (check (cdr (assoc 'role resp1)) => "assistant")
    (check (cdr (assoc 'stopReason resp1)) => "stop")
    (check (cdr (assoc 'text (car (cdr (assoc 'content resp1))))) => "Hello from stub!")

    ;; 3.2 functionCall
    (let* ((cfg-fc (list (cons 'provider "google")
                         (cons 'api-key "test-key-stub-123")
                         (cons 'model-id "function-call")
                         (cons 'model-meta (list (cons 'base-url base-url)))
                         (cons 'proxy-url #f)
                         (cons 'tools '("bash"))
                         (cons 'settings '())))
           (resp-fc (gemini-request cfg-fc (list '((role . "user") (content . "run ls"))) #f '("bash") 10)))
      (check (cdr (assoc 'stopReason resp-fc)) => "toolUse")
      (let ((blk (car (cdr (assoc 'content resp-fc)))))
        (check (cdr (assoc 'name blk)) => "bash")
        (check (cdr (assoc 'id blk)) => "bash_call_1")))

    ;; 3.3 MAX_TOKENS
    (let* ((cfg-len (list (cons 'provider "google")
                          (cons 'api-key "test-key-stub-123")
                          (cons 'model-id "max-tokens")
                          (cons 'model-meta (list (cons 'base-url base-url)))
                          (cons 'proxy-url #f)
                          (cons 'tools '())
                          (cons 'settings '())))
           (resp-len (gemini-request cfg-len (list '((role . "user") (content . "more"))) #f '() 10)))
      (check (cdr (assoc 'stopReason resp-len)) => "length"))

    ;; 3.4 SAFETY
    (let* ((cfg-safe (list (cons 'provider "google")
                           (cons 'api-key "test-key-stub-123")
                           (cons 'model-id "safety")
                           (cons 'model-meta (list (cons 'base-url base-url)))
                           (cons 'proxy-url #f)
                           (cons 'tools '())
                           (cons 'settings '())))
           (resp-safe (gemini-request cfg-safe (list '((role . "user") (content . "unsafe"))) #f '() 10)))
      (check (cdr (assoc 'stopReason resp-safe)) => "error")
      (check (cdr (assoc 'errorMessage resp-safe)) => "SAFETY"))

    ;; 3.5 429 退避重试并最终成功（3次尝试：前两次429，第3次ok）
    (let* ((cfg-retry (list (cons 'provider "google")
                            (cons 'api-key "test-key-stub-123")
                            (cons 'model-id "429-then-ok")
                            (cons 'model-meta (list (cons 'base-url base-url)))
                            (cons 'proxy-url #f)
                            (cons 'tools '())
                            (cons 'settings '())))
           (retry-opt '((enabled . #t) (max-retries . 3) (base-delay-ms . 50) (max-agent-delay-ms . 200)))
           (resp-retry (gemini-request cfg-retry (list '((role . "user") (content . "retry me"))) #f '() 10 retry-opt)))
      (check (cdr (assoc 'stopReason resp-retry)) => "stop")
      (check (cdr (assoc 'text (car (cdr (assoc 'content resp-retry))))) => "Hello from stub!"))

    ;; 3.5b provider 级预算：顶层 max-retries=3 但 provider-max-retries=8，
    ;;    前 5 次 429 后成功（旧行为下第 4 次尝试即快速失败）
    (let* ((cfg-retry-p (list (cons 'provider "google")
                              (cons 'api-key "test-key-stub-123")
                              (cons 'model-id "429-5-then-ok")
                              (cons 'model-meta (list (cons 'base-url base-url)))
                              (cons 'proxy-url #f)
                              (cons 'tools '())
                              (cons 'settings '())))
           (retry-opt-p '((enabled . #t) (max-retries . 3) (provider-max-retries . 8)
                          (base-delay-ms . 20) (max-agent-delay-ms . 100)))
           (resp-retry-p (gemini-request cfg-retry-p
                           (list '((role . "user") (content . "retry me")))
                           #f '() 10 retry-opt-p)))
      (check (cdr (assoc 'stopReason resp-retry-p)) => "stop")
      (check (cdr (assoc 'text (car (cdr (assoc 'content resp-retry-p))))) => "Hello from stub!"))

    ;; 3.5c 无 provider 预算：429 持续时按顶层 max-retries=2 快速失败并报错
    (let* ((cfg-retry-f (list (cons 'provider "google")
                              (cons 'api-key "test-key-stub-123")
                              (cons 'model-id "429-always")
                              (cons 'model-meta (list (cons 'base-url base-url)))
                              (cons 'proxy-url #f)
                              (cons 'tools '())
                              (cons 'settings '())))
           (retry-opt-f '((enabled . #t) (max-retries . 2) (base-delay-ms . 10)
                          (max-agent-delay-ms . 50)))
           (resp-retry-f (gemini-request cfg-retry-f
                           (list '((role . "user") (content . "retry me")))
                           #f '() 10 retry-opt-f)))
      (check (cdr (assoc 'stopReason resp-retry-f)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-retry-f))
                     "Too Many Requests")))

    ;; 3.6 SSE 流式调用 (gemini-request-stream)
    (let* ((cfg-stream (list (cons 'provider "google")
                             (cons 'api-key "test-key-stub-123")
                             (cons 'model-id "gemini-3.8-flash")
                             (cons 'model-meta (list (cons 'base-url base-url)))
                             (cons 'proxy-url #f)
                             (cons 'tools '("read" "bash"))
                             (cons 'settings '())))
           (deltas '())
           (stream-events '())
           (on-event (lambda (ev)
                       (set! stream-events (cons ev stream-events))
                       (when (equal? (cdr (assoc 'type ev)) "text_delta")
                         (set! deltas (cons (cdr (assoc 'delta ev)) deltas)))))
           (resp-stream (gemini-request-stream cfg-stream
                                               (list '((role . "user") (content . "Hello stream")))
                                               #f '("bash") on-event 10)))
      (check (cdr (assoc 'role resp-stream)) => "assistant")
      (check (cdr (assoc 'stopReason resp-stream)) => "stop")
      (check (reverse deltas) => '("Hello " "stream!"))
      (check (cdr (assoc 'text (car (cdr (assoc 'content resp-stream))))) => "Hello stream!")
      (let ((u (cdr (assoc 'usage resp-stream))))
        (check (cdr (assoc 'input u)) => 10)
        (check (cdr (assoc 'output u)) => 6)
        (check (cdr (assoc 'totalTokens u)) => 16)))

    ;; 3.7 SSE 流式空闲超时 (idle-timeout)
    (let* ((cfg-idle (list (cons 'provider "google")
                           (cons 'api-key "test-key-stub-123")
                           (cons 'model-id "stream-idle-hang")
                           (cons 'model-meta (list (cons 'base-url base-url)))
                           (cons 'proxy-url #f)
                           (cons 'tools '())
                           (cons 'settings '())))
           (on-event (lambda (ev) #f))
           (resp-idle (gemini-request-stream cfg-idle
                                             (list '((role . "user") (content . "hang")))
                                             #f '() on-event 10 #f 1)))
      (check (cdr (assoc 'stopReason resp-idle)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-idle)) "idle timed out")))

    ;; 3.8 HTTP 400 错误体透传与摘要输出
    (let* ((cfg-400 (list (cons 'provider "google")
                          (cons 'api-key "test-key-stub-123")
                          (cons 'model-id "400")
                          (cons 'model-meta (list (cons 'base-url base-url)))
                          (cons 'proxy-url #f)
                          (cons 'tools '())
                          (cons 'settings '())))
           (resp-400 (gemini-request-stream cfg-400
                                           (list '((role . "user") (content . "cause-400")))
                                           #f '() (lambda (ev) #f) 10)))
      (check (cdr (assoc 'stopReason resp-400)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-400)) "HTTP 400:"))
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-400)) "system_instruction")))

    ;; 3.9 无效代理错误提示人类可读原因 (status 0)
    (let* ((cfg-proxy (list (cons 'provider "google")
                            (cons 'api-key "test-key-stub-123")
                            (cons 'model-id "normal")
                            (cons 'model-meta (list (cons 'base-url base-url)))
                            (cons 'proxy-url "http://127.0.0.1:9999")
                            (cons 'tools '())
                            (cons 'settings '((retry (enabled . #f))))))
           (resp-proxy (gemini-request-stream cfg-proxy
                                              (list '((role . "user") (content . "test")))
                                              #f '() (lambda (ev) #f) 2 #f 1)))
      (check (cdr (assoc 'stopReason resp-proxy)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-proxy)) "Request failed:"))
      (check-false (string=? (cdr (assoc 'errorMessage resp-proxy)) "HTTP 0 stream error")))
  ) ;let*

  ;; 清理子进程
  (process-kill proc)
  (process-wait proc)
) ;let*

(check-report)
