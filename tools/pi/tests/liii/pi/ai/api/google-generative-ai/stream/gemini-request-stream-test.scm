(import (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi ai event)
  (liii pi ai api google-generative-ai)
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
        ) ;

    ;; SSE 流式调用
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

    ;; SSE 流式空闲超时
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

    ;; HTTP 400 错误体透传与摘要输出
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

    ;; 无效代理错误提示人类可读原因 (status 0)
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

  (process-kill proc)
  (process-wait proc)
) ;let*

(check-report)
