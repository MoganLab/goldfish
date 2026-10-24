(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi ai anthropic) (liii pi ai event)
  (liii pi config)
  (liii pi tools)
) ;import

(check-set-mode! 'report-failed)

;; 1. anthropic-build-tool-declarations-from-names
(let ((decls (anthropic-build-tool-declarations-from-names '("bash" "read") get-tool-definition)))
  (check (length decls) => 2)
  (check (cdr (assoc "name" (car decls))) => "bash")
  (check (cdr (assoc "name" (cadr decls))) => "read")
) ;let

;; 2. Stub server integration tests

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define stub-server-file
  (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
    "tools/pi/tests/stub_gemini_server.py"
    "tests/stub_gemini_server.py"
  ) ;if
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (check-true (number? port))

  (let ((config `((provider . "anthropic") (model-id . "claude-3-7-sonnet-20250219")
                        (api-key . "sk-ant-test-key") (model-meta (base-url . ,base-url) (max-tokens . 4096))
                        (proxy-url . #f) (tools "bash")
                        (settings (retry (enabled . #t) (max-retries . 3)
                                         (base-delay-ms . 10) (max-agent-delay-ms . 50))))))

    ;; 2.1 基础普通消息请求与回显验证
    (let* ((msgs '(((role . "user") (content . "hello claude"))))
           (resp (anthropic-request config msgs "You are pi" '("bash") 10))
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "stop")
      (let* ((content (cdr (assoc 'content resp))) (first-blk (car content)))
        (check (cdr (assoc 'type first-blk)) => "text")
        (check (cdr (assoc 'text first-blk)) => "Hello from Claude stub!")
      ) ;let*
    ) ;let*

    ;; 2.2 工具调用响应测试
    (let* ((call-cfg
             `((provider . "anthropic")
               (model-id . "claude-3-7-sonnet-20250219")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/tool-call"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;call-cfg
           (msgs '(((role . "user") (content . "run echo"))))
           (resp (anthropic-request call-cfg msgs #f '("bash") 10))
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "toolUse")
      (let* ((content (cdr (assoc 'content resp))) (first-blk (car content)))
        (check (cdr (assoc 'type first-blk)) => "toolCall")
        (check (cdr (assoc 'id first-blk)) => "ant_call_1")
        (check (cdr (assoc 'name first-blk)) => "bash")
      ) ;let*
    ) ;let*

    ;; 2.3 MAX_TOKENS 截断响应
    (let* ((trunc-cfg
             `((provider . "anthropic")
               (model-id . "claude-3-7-sonnet-20250219")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/max-tokens"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;trunc-cfg
           (resp
             (anthropic-request trunc-cfg
               '(((role . "user") (content . "long text")))
               #f
               '()
               10
             ) ;anthropic-request
           ) ;resp
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "length")
    ) ;let*

    ;; 2.4 HTTP 429 退避重试成功
    (let* ((retry-cfg
             `((provider . "anthropic")
               (model-id . "claude-3-7-sonnet-20250219")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/429-then-ok"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings (retry (enabled . #t)
                           (max-retries . 3)
                           (base-delay-ms . 10)
                           (max-agent-delay-ms . 50))))
           ) ;retry-cfg
           (resp
             (anthropic-request retry-cfg
               '(((role . "user") (content . "retry test")))
               #f
               '()
               10
             ) ;anthropic-request
           ) ;resp
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "stop")
    ) ;let*

    ;; 2.5 HTTP 500 超过重试次数返回 error
    (let* ((err-cfg
             `((provider . "anthropic")
               (model-id . "claude-3-7-sonnet-20250219")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/error-500"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings (retry (enabled . #t)
                           (max-retries . 2)
                           (base-delay-ms . 10)
                           (max-agent-delay-ms . 30))))
           ) ;err-cfg
           (resp
             (anthropic-request err-cfg
               '(((role . "user") (content . "500 test")))
               #f
               '()
               10
             ) ;anthropic-request
           ) ;resp
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "error")
    ) ;let*

    ;; 2.6 Anthropic SSE 流式请求与 text_delta 事件捕获
    (let* ((evs '())
           (stream-cfg
             `((provider . "anthropic")
               (model-id . "claude-3-7-sonnet-20250219")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/stream"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;stream-cfg
           (on-ev (lambda (ev) (set! evs (cons ev evs))))
           (resp
             (anthropic-request-stream stream-cfg
               '(((role . "user") (content . "stream please")))
               #f
               '("bash")
               on-ev
               10
             ) ;anthropic-request-stream
           ) ;resp
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "stop")
      (let* ((content (cdr (assoc 'content resp))) (first-blk (car content)))
        (check (cdr (assoc 'type first-blk)) => "text")
        (check (cdr (assoc 'text first-blk)) => "Hello from Claude stream!")
      ) ;let*
      (let ((delta-evs
              (filter
                (lambda (e) (equal? (cdr (assoc 'type e)) "text_delta"))
                evs
              ) ;filter
            ) ;delta-evs
           ) ;
        (check (> (length delta-evs) 0) => #t)
      ) ;let
    ) ;let*

    ;; 2.7 Anthropic 兼容网关测试（含 thinking 块、流式与非流式）
    ;; 2.7.1 兼容网关流式
    (let* ((evs '())
           (compat-stream-cfg
             `((provider . "anthropic")
               (model-id . "glm-5-turbo")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url
                             . ,(string-append base-url "/compat-gateway"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;compat-stream-cfg
           (on-ev (lambda (ev) (set! evs (cons ev evs))))
           (resp
             (anthropic-request-stream compat-stream-cfg
               '(((role . "user") (content . "compat stream")))
               #f
               '("bash")
               on-ev
               10
             ) ;anthropic-request-stream
           ) ;resp
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "stop")
      (let* ((content (cdr (assoc 'content resp)))
             (first-blk (car content))
             (second-blk (cadr content)))
        (check (length content) => 2)
        (check (cdr (assoc 'type first-blk)) => "thinking")
        (check (cdr (assoc 'thinking first-blk)) => "Thinking process...")
        (check (cdr (assoc 'thinkingSignature first-blk)) => "sig_abc")
        (check (cdr (assoc 'type second-blk)) => "text")
        (check (cdr (assoc 'text second-blk)) => "Hello from compatible gateway!")
      ) ;let*
      (let ((u (cdr (assoc 'usage resp))))
        (check (cdr (assoc 'input u)) => 15)
        (check (cdr (assoc 'output u)) => 8)
        (check (cdr (assoc 'cacheRead u)) => 4)
      ) ;let
    ) ;let*

    ;; 2.7.2 兼容网关非流式
    (let* ((compat-cfg `((provider . "anthropic") (model-id . "glm-5-turbo")
                         (api-key . "sk-ant-test-key")
                         (model-meta (base-url . ,(string-append base-url "/compat-gateway")) (max-tokens . 4096))
                         (proxy-url . #f) (tools "bash") (settings)))
           (resp (anthropic-request compat-cfg '(((role . "user") (content . "compat nonstream"))) #f '("bash") 10)))
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "stop")
      (let* ((content (cdr (assoc 'content resp)))
             (first-blk (car content))
             (second-blk (cadr content)))
        (check (length content) => 2)
        (check (cdr (assoc 'type first-blk)) => "thinking")
        (check (cdr (assoc 'thinking first-blk)) => "Thinking non-stream")
        (check (cdr (assoc 'thinkingSignature first-blk)) => "sig_1")
        (check (cdr (assoc 'type second-blk)) => "text")
        (check (cdr (assoc 'text second-blk)) => "Hello from compatible gateway non-stream!"))
      (let ((u (cdr (assoc 'usage resp))))
        (check (cdr (assoc 'input u)) => 15)
        (check (cdr (assoc 'output u)) => 8)
        (check (cdr (assoc 'cacheRead u)) => 4)))

    ;; 2.8 错误路径测试：400 报错时返回 stopReason=error 并包含错误信息
    (let* ((err-stream-cfg `((provider . "anthropic") (model-id . "fake-model")
                            (api-key . "sk-ant-test-key")
                            (model-meta (base-url . ,(string-append base-url "/error-400")) (max-tokens . 4096))
                            (proxy-url . #f) (tools "bash") (settings)))
           (resp (anthropic-request-stream err-stream-cfg '(((role . "user") (content . "cause 400"))) #f '("bash") #f 10)))
      (check (cdr (assoc 'stopReason resp)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp)) "model not found")))
  ) ;let

  (process-kill proc)
) ;let*

(check-report)
