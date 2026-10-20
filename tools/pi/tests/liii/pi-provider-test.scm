(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi)
  (liii pi config)
  (liii pi gemini)
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
(let* ((resp-json '(("candidates"
                     . #((("content" ("parts" . #((("text" . "Hello world")))))
                          ("finishReason" . "STOP"))
                       ) ;#
                    )
                    ("usageMetadata"
                     ("promptTokenCount" . 10)
                     ("candidatesTokenCount" . 5)
                     ("totalTokenCount" . 15)))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 1)
    (check (cdr (assoc 'text (car content))) => "Hello world")
  ) ;let
  (let ((u (cdr (assoc 'usage parsed))))
    (check (cdr (assoc 'input u)) => 10)
    (check (cdr (assoc 'output u)) => 5)
  ) ;let
) ;let*

;; functionCall 回复
(let* ((resp-json '(("candidates"
                     . #((("content"
                           ("parts"
                            . #((("functionCall"
                                  ("name" . "bash")
                                  ("args" ("command" . "ls"))
                                  ("id" . "call_123")))
                              ) ;#
                           ))
                          ("finishReason" . "STOP"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (cdr (assoc 'stopReason parsed)) => "toolUse")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 1)
    (let ((blk (car content)))
      (check (cdr (assoc 'type blk)) => "toolCall")
      (check (cdr (assoc 'id blk)) => "call_123")
      (check (cdr (assoc 'name blk)) => "bash")
      (check
        (cdr (assoc "command" (cdr (assoc 'arguments blk))))
        =>
        "ls"
      ) ;check
    ) ;let
  ) ;let
) ;let*

;; MAX_TOKENS 截断
(let* ((resp-json '(("candidates"
                     . #((("content" ("parts" . #((("text" . "partial")))))
                          ("finishReason" . "MAX_TOKENS"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (cdr (assoc 'stopReason parsed)) => "length")
) ;let*

;; SAFETY 报错
(let* ((resp-json '(("candidates"
                     . #((("content" ("parts" . #()))
                          ("finishReason" . "SAFETY"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (cdr (assoc 'stopReason parsed)) => "error")
  (check (cdr (assoc 'errorMessage parsed)) => "SAFETY")
) ;let*

;; 带有 thinking part 的响应解析
(let* ((resp-json '(("candidates"
                     . #((("content"
                           ("parts"
                            . #((("text" . "Let me ponder...")
                                 ("thought" . #t)
                                 ("thoughtSignature" . "sig-abc"))
                                (("text" . "The final answer."))
                              ) ;#
                           ))
                          ("finishReason" . "STOP"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 2)
    (let ((b1 (car content)) (b2 (cadr content)))
      (check (cdr (assoc 'type b1)) => "thinking")
      (check (cdr (assoc 'thinking b1)) => "Let me ponder...")
      (check (cdr (assoc 'thinkingSignature b1)) => "sig-abc")
      (check (cdr (assoc 'type b2)) => "text")
      (check (cdr (assoc 'text b2)) => "The final answer.")
    ) ;let
  ) ;let
) ;let*

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

  (let* ((cfg
           (list (cons 'provider "google")
             (cons 'api-key "test-key-stub-123")
             (cons 'model-id "gemini-3.8-flash")
             (cons 'model-meta (list (cons 'base-url base-url)))
             (cons 'proxy-url #f)
             (cons 'tools '("read" "bash"))
             (cons 'settings '())
           ) ;list
         ) ;cfg
         ;; 3.1 正常调用
         (resp1
           (gemini-request cfg
             (list '((role . "user") (content . "Hello")))
             "System prompt"
             '("bash")
             10
           ) ;gemini-request
         ) ;resp1
        ) ;
    (check (cdr (assoc 'role resp1)) => "assistant")
    (check (cdr (assoc 'stopReason resp1)) => "stop")
    (check
      (cdr
        (assoc 'text (car (cdr (assoc 'content resp1))))
      ) ;cdr
      =>
      "Hello from stub!"
    ) ;check

    ;; 3.2 functionCall
    (let* ((cfg-fc
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "function-call")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '("bash"))
               (cons 'settings '())
             ) ;list
           ) ;cfg-fc
           (resp-fc
             (gemini-request cfg-fc
               (list '((role . "user") (content . "run ls")))
               #f
               '("bash")
               10
             ) ;gemini-request
           ) ;resp-fc
          ) ;
      (check (cdr (assoc 'stopReason resp-fc)) => "toolUse")
      (let ((blk (car (cdr (assoc 'content resp-fc)))))
        (check (cdr (assoc 'name blk)) => "bash")
        (check (cdr (assoc 'id blk)) => "bash_call_1")
      ) ;let
    ) ;let*

    ;; 3.3 MAX_TOKENS
    (let* ((cfg-len
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "max-tokens")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '())
               (cons 'settings '())
             ) ;list
           ) ;cfg-len
           (resp-len
             (gemini-request cfg-len (list '((role . "user") (content . "more"))) #f '() 10)
           ) ;resp-len
          ) ;
      (check (cdr (assoc 'stopReason resp-len)) => "length")
    ) ;let*

    ;; 3.4 SAFETY
    (let* ((cfg-safe
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "safety")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '())
               (cons 'settings '())
             ) ;list
           ) ;cfg-safe
           (resp-safe
             (gemini-request cfg-safe
               (list '((role . "user") (content . "unsafe")))
               #f
               '()
               10
             ) ;gemini-request
           ) ;resp-safe
          ) ;
      (check (cdr (assoc 'stopReason resp-safe)) => "error")
      (check (cdr (assoc 'errorMessage resp-safe)) => "SAFETY")
    ) ;let*

    ;; 3.5 429 退避重试并最终成功（3次尝试：前两次429，第3次ok）
    (let* ((cfg-retry
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "429-then-ok")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '())
               (cons 'settings '())
             ) ;list
           ) ;cfg-retry
           (retry-opt '((enabled . #t)
                        (max-retries . 3)
                        (base-delay-ms . 50)
                        (max-agent-delay-ms . 200))
           ) ;retry-opt
           (resp-retry
             (gemini-request cfg-retry
               (list '((role . "user") (content . "retry me")))
               #f
               '()
               10
               retry-opt
             ) ;gemini-request
           ) ;resp-retry
          ) ;
      (check (cdr (assoc 'stopReason resp-retry)) => "stop")
      (check
        (cdr
          (assoc 'text (car (cdr (assoc 'content resp-retry))))
        ) ;cdr
        =>
        "Hello from stub!"
      ) ;check
    ) ;let*

    ;; 3.5b provider 级预算：顶层 max-retries=3 但 provider-max-retries=8，
    ;;    前 5 次 429 后成功（旧行为下第 4 次尝试即快速失败）
    (let* ((cfg-retry-p
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "429-5-then-ok")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '())
               (cons 'settings '())
             ) ;list
           ) ;cfg-retry-p
           (retry-opt-p '((enabled . #t)
                          (max-retries . 3)
                          (provider-max-retries . 8)
                          (base-delay-ms . 20)
                          (max-agent-delay-ms . 100))
           ) ;retry-opt-p
           (resp-retry-p
             (gemini-request cfg-retry-p
               (list '((role . "user") (content . "retry me")))
               #f
               '()
               10
               retry-opt-p
             ) ;gemini-request
           ) ;resp-retry-p
          ) ;
      (check (cdr (assoc 'stopReason resp-retry-p)) => "stop")
      (check
        (cdr
          (assoc 'text (car (cdr (assoc 'content resp-retry-p))))
        ) ;cdr
        =>
        "Hello from stub!"
      ) ;check
    ) ;let*

    ;; 3.5c 无 provider 预算：429 持续时按顶层 max-retries=2 快速失败并报错
    (let* ((cfg-retry-f
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "429-always")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '())
               (cons 'settings '())
             ) ;list
           ) ;cfg-retry-f
           (retry-opt-f '((enabled . #t)
                          (max-retries . 2)
                          (base-delay-ms . 10)
                          (max-agent-delay-ms . 50))
           ) ;retry-opt-f
           (resp-retry-f
             (gemini-request cfg-retry-f
               (list '((role . "user") (content . "retry me")))
               #f
               '()
               10
               retry-opt-f
             ) ;gemini-request
           ) ;resp-retry-f
          ) ;
      (check (cdr (assoc 'stopReason resp-retry-f)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-retry-f)) "Too Many Requests")
      ) ;check-true
    ) ;let*

    ;; 3.6 SSE 流式调用 (gemini-request-stream)
    (let* ((cfg-stream
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "gemini-3.8-flash")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '("read" "bash"))
               (cons 'settings '())
             ) ;list
           ) ;cfg-stream
           (deltas '())
           (stream-events '())
           (on-event
             (lambda (ev)
               (set! stream-events (cons ev stream-events))
               (when (equal? (cdr (assoc 'type ev)) "text_delta")
                 (set! deltas (cons (cdr (assoc 'delta ev)) deltas))
               ) ;when
             ) ;lambda
           ) ;on-event
           (resp-stream
             (gemini-request-stream cfg-stream
               (list '((role . "user") (content . "Hello stream")))
               #f
               '("bash")
               on-event
               10
             ) ;gemini-request-stream
           ) ;resp-stream
          ) ;
      (check (cdr (assoc 'role resp-stream)) => "assistant")
      (check (cdr (assoc 'stopReason resp-stream)) => "stop")
      (check (reverse deltas) => '("Hello " "stream!"))
      (check
        (cdr
          (assoc 'text (car (cdr (assoc 'content resp-stream))))
        ) ;cdr
        =>
        "Hello stream!"
      ) ;check
      (let ((u (cdr (assoc 'usage resp-stream))))
        (check (cdr (assoc 'input u)) => 10)
        (check (cdr (assoc 'output u)) => 6)
        (check (cdr (assoc 'totalTokens u)) => 16)
      ) ;let
    ) ;let*

    ;; 3.7 SSE 流式空闲超时 (idle-timeout)
    (let* ((cfg-idle
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "stream-idle-hang")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '())
               (cons 'settings '())
             ) ;list
           ) ;cfg-idle
           (on-event (lambda (ev) #f))
           (resp-idle
             (gemini-request-stream cfg-idle
               (list '((role . "user") (content . "hang")))
               #f
               '()
               on-event
               10
               #f
               1
             ) ;gemini-request-stream
           ) ;resp-idle
          ) ;
      (check (cdr (assoc 'stopReason resp-idle)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-idle)) "idle timed out")
      ) ;check-true
    ) ;let*

    ;; 3.8 HTTP 400 错误体透传与摘要输出
    (let* ((cfg-400
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "400")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url #f)
               (cons 'tools '())
               (cons 'settings '())
             ) ;list
           ) ;cfg-400
           (resp-400
             (gemini-request-stream cfg-400
               (list '((role . "user") (content . "cause-400")))
               #f
               '()
               (lambda (ev) #f)
               10
             ) ;gemini-request-stream
           ) ;resp-400
          ) ;
      (check (cdr (assoc 'stopReason resp-400)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-400)) "HTTP 400:"))
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-400)) "system_instruction")
      ) ;check-true
    ) ;let*

    ;; 3.9 无效代理错误提示人类可读原因 (status 0)
    (let* ((cfg-proxy
             (list (cons 'provider "google")
               (cons 'api-key "test-key-stub-123")
               (cons 'model-id "normal")
               (cons 'model-meta (list (cons 'base-url base-url)))
               (cons 'proxy-url "http://127.0.0.1:9999")
               (cons 'tools '())
               (cons 'settings '((retry (enabled . #f))))
             ) ;list
           ) ;cfg-proxy
           (resp-proxy
             (gemini-request-stream cfg-proxy
               (list '((role . "user") (content . "test")))
               #f
               '()
               (lambda (ev) #f)
               2
               #f
               1
             ) ;gemini-request-stream
           ) ;resp-proxy
          ) ;
      (check (cdr (assoc 'stopReason resp-proxy)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp-proxy)) "Request failed:")
      ) ;check-true
      (check-false (string=? (cdr (assoc 'errorMessage resp-proxy)) "HTTP 0 stream error")
      ) ;check-false
    ) ;let*
  ) ;let*

  ;; 清理子进程
  (process-kill proc)
  (process-wait proc)
) ;let*

;; 4. 多协议 toolResult 格式转换测试（支持 vector 与 list 两种形态的 content，防止恢复后落入空串 fallback）
(let* ((vec-msg (pi-tool-result-message "call_b1"
                  "bash"
                  (vector (pi-text-block "SECRET-1071"))
                  #f
                ) ;pi-tool-result-message
       ) ;vec-msg
       (raw-vec-msg (pi-tool-result-message "call_r1"
                      "read"
                      '#((("type" . "text") ("text" . "FILE-CONTENT-1071")))
                      #f
                    ) ;pi-tool-result-message
       ) ;raw-vec-msg
       (list-msg (pi-tool-result-message "call_l1" "read" (list (pi-text-block "LIST-1071")) #f)
       ) ;list-msg
      ) ;

  ;; 4.1 Anthropic 协议转换
  (let* ((ant-vec (anthropic-build-messages (list vec-msg)))
         (ant-raw (anthropic-build-messages (list raw-vec-msg)))
         (ant-list (anthropic-build-messages (list list-msg)))
        ) ;
    (check (vector-length ant-vec) => 1)
    (let* ((turn (vector-ref ant-vec 0)) (blks (cdr (assoc "content" turn))))
      (check (vector-length blks) => 1)
      (check (cdr (assoc "type" (vector-ref blks 0))) => "tool_result")
      (check (cdr (assoc "tool_use_id" (vector-ref blks 0))) => "call_b1")
      (check (cdr (assoc "content" (vector-ref blks 0))) => "SECRET-1071")
    ) ;let*
    (let* ((turn (vector-ref ant-raw 0)) (blks (cdr (assoc "content" turn))))
      (check (cdr (assoc "content" (vector-ref blks 0))) => "FILE-CONTENT-1071")
    ) ;let*
    (let* ((turn (vector-ref ant-list 0)) (blks (cdr (assoc "content" turn))))
      (check (cdr (assoc "content" (vector-ref blks 0))) => "LIST-1071")
    ) ;let*
  ) ;let*

  ;; 4.2 Kimi 协议转换
  (let* ((kimi-vec (kimi-build-messages (list vec-msg) #f))
         (kimi-raw (kimi-build-messages (list raw-vec-msg) #f))
         (kimi-list (kimi-build-messages (list list-msg) #f))
        ) ;
    (check (vector-length kimi-vec) => 1)
    (let ((m (vector-ref kimi-vec 0)))
      (check (cdr (assoc "role" m)) => "tool")
      (check (cdr (assoc "tool_call_id" m)) => "call_b1")
      (check (cdr (assoc "content" m)) => "SECRET-1071")
    ) ;let
    (let ((m (vector-ref kimi-raw 0)))
      (check (cdr (assoc "role" m)) => "tool")
      (check (cdr (assoc "tool_call_id" m)) => "call_r1")
      (check (cdr (assoc "content" m)) => "FILE-CONTENT-1071")
    ) ;let
    (let ((m (vector-ref kimi-list 0)))
      (check (cdr (assoc "role" m)) => "tool")
      (check (cdr (assoc "tool_call_id" m)) => "call_l1")
      (check (cdr (assoc "content" m)) => "LIST-1071")
    ) ;let
  ) ;let*

  ;; 4.3 Gemini 协议转换
  (let* ((gem-vec (gemini-build-contents (list vec-msg)))
         (gem-raw (gemini-build-contents (list raw-vec-msg)))
         (gem-list (gemini-build-contents (list list-msg)))
        ) ;
    (check (vector-length gem-vec) => 1)
    (let* ((turn (vector-ref gem-vec 0))
           (parts (cdr (assoc "parts" turn)))
           (fr (cdr (assoc "functionResponse" (vector-ref parts 0))))
           (resp (cdr (assoc "response" fr)))
          ) ;
      (check (cdr (assoc "name" fr)) => "bash")
      (check (cdr (assoc "id" fr)) => "call_b1")
      (check (cdr (assoc "output" resp)) => "SECRET-1071")
    ) ;let*
    (let* ((turn (vector-ref gem-raw 0))
           (parts (cdr (assoc "parts" turn)))
           (fr (cdr (assoc "functionResponse" (vector-ref parts 0))))
           (resp (cdr (assoc "response" fr)))
          ) ;
      (check (cdr (assoc "output" resp)) => "FILE-CONTENT-1071")
    ) ;let*
    (let* ((turn (vector-ref gem-list 0))
           (parts (cdr (assoc "parts" turn)))
           (fr (cdr (assoc "functionResponse" (vector-ref parts 0))))
           (resp (cdr (assoc "response" fr)))
          ) ;
      (check (cdr (assoc "output" resp)) => "LIST-1071")
    ) ;let*
  ) ;let*

  ;; 4.4 DeepSeek 协议转换
  (let* ((ds-vec (deepseek-build-messages (list vec-msg)))
         (ds-raw (deepseek-build-messages (list raw-vec-msg)))
         (ds-list (deepseek-build-messages (list list-msg)))
        ) ;
    (check (vector-length ds-vec) => 1)
    (let* ((turn (vector-ref ds-vec 0)) (blks (cdr (assoc "content" turn))))
      (check (cdr (assoc "content" (vector-ref blks 0))) => "SECRET-1071")
    ) ;let*
    (let* ((turn (vector-ref ds-raw 0)) (blks (cdr (assoc "content" turn))))
      (check (cdr (assoc "content" (vector-ref blks 0))) => "FILE-CONTENT-1071")
    ) ;let*
    (let* ((turn (vector-ref ds-list 0)) (blks (cdr (assoc "content" turn))))
      (check (cdr (assoc "content" (vector-ref blks 0))) => "LIST-1071")
    ) ;let*
  ) ;let*
) ;let*

(check-report)
