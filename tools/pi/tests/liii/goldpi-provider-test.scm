(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi)
  (liii goldpi-config)
  (liii goldpi-provider)
) ;import

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

;; 3. 端到端测试：本地 stub server
(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define stub-server-file
  (if (file-exists? "tests/stub_gemini_server.py")
    "tests/stub_gemini_server.py"
    "tools/pi/tests/stub_gemini_server.py"
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
  ) ;let*

  ;; 清理子进程
  (process-kill proc)
  (process-wait proc)
) ;let*

(check-report)
