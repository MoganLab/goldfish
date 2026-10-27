(import (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii subprocess)
  (liii pi ai event)
  (liii pi ai api anthropic-messages)
) ;import

(check-set-mode! 'report-failed)

;; 本地 stub server 集成测试

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

  (let ((config
          `((provider . "anthropic")
            (model-id . "claude-3-7-sonnet-20250219")
            (api-key . "sk-ant-test-key")
            (model-meta (base-url . ,base-url) (max-tokens . 4096))
            (proxy-url . #f)
            (tools "bash")
            (settings (retry (enabled . #t)
                        (max-retries . 3)
                        (base-delay-ms . 10)
                        (max-agent-delay-ms . 50))))
        ) ;config
       ) ;

    ;; Anthropic SSE 流式请求与 text_delta 事件捕获
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

    ;; Anthropic 兼容网关流式（含 thinking 块）
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
             (second-blk (cadr content))
            ) ;
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

    ;; 400 报错时返回 stopReason=error 并包含错误信息
    (let* ((err-stream-cfg
             `((provider . "anthropic")
               (model-id . "fake-model")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/error-400"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;err-stream-cfg
           (resp
             (anthropic-request-stream err-stream-cfg
               '(((role . "user") (content . "cause 400")))
               #f
               '("bash")
               #f
               10
             ) ;anthropic-request-stream
           ) ;resp
          ) ;
      (check (cdr (assoc 'stopReason resp)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp)) "model not found")
      ) ;check-true
    ) ;let*
  ) ;let

  (process-kill proc)
) ;let*

(check-report)
