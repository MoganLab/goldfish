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

    ;; functionCall
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

    ;; MAX_TOKENS
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

    ;; SAFETY
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

    ;; 429 退避重试并最终成功
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

    ;; provider 级预算：前 5 次 429 后成功
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

    ;; 无 provider 预算：429 持续时按顶层 max-retries=2 快速失败
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
  ) ;let*

  (process-kill proc)
  (process-wait proc)
) ;let*

(check-report)
