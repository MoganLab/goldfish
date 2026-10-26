(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii sys)
  (liii pi code cli)
)

(check-set-mode! 'report-failed)

;; 任务 033 e2e：multikey 密钥池 429/403 自动轮换（Gemini stub server）
;; fixture multikey-e2e：pool 含 mk-gem-key-1-*（被拒）与 mk-gem-key-2-*（放行），
;; cooldownMs=1ms 保证既有重试换 key 后立即可用。

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
  (if (file-exists? "tests/fixtures/multikey-e2e")
    "tests/fixtures/multikey-e2e"
    (if (file-exists? "tools/pi/tests/fixtures/multikey-e2e")
      "tools/pi/tests/fixtures/multikey-e2e"
      "../../tools/pi/tests/fixtures/multikey-e2e"
    ) ;if
  ) ;if
) ;define

(define FIXTURE_NOPOOL
  (if (file-exists? "tests/fixtures/multikey-nopool")
    "tests/fixtures/multikey-nopool"
    (if (file-exists? "tools/pi/tests/fixtures/multikey-nopool")
      "tools/pi/tests/fixtures/multikey-nopool"
      "../../tools/pi/tests/fixtures/multikey-nopool"
    ) ;if
  ) ;if
) ;define

(define GF_BIN (executable))

(define (run-pi agent-dir base-url model provider)
  (let* ((test-env (append (get-environment-variables)
                     (list (cons "PI_CODING_AGENT_DIR" agent-dir)
                       (cons "GF_PI_BASE_URL" base-url)
                       (cons "GF_PI_MODEL" model)
                       (cons "https_proxy" "")
                       (cons "http_proxy" "")
                       (cons "all_proxy" "")
                       (cons "ALL_PROXY" "")
                       (cons "HTTPS_PROXY" "")
                       (cons "HTTP_PROXY" "")
                       (cons "no_proxy" "")
                       (cons "NO_PROXY" "")
                       (cons "ANTHROPIC_BASE_URL" "")
                       (cons "ANTHROPIC_API_KEY" "")
                       (cons "ANTHROPIC_AUTH_TOKEN" "")
                       (cons "DEEPSEEK_API_KEY" "")
                       (cons "DEEPSEEK_BASE_URL" "")
                     ) ;list
                   ) ;append
         ) ;test-env
         (cmd (append (list GF_BIN "pi" "--print" "--no-session" "--timeout" "20")
                (if provider (list "--provider" provider "--model" model) '())
                (list "multikey rotation test")
              ) ;append
         ) ;cmd
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 30000))
        ) ;
    (unless code
      (process-kill p)
      (process-wait p 2000)
    ) ;unless
    (let ((out (process-read p :stream 'stdout))
          (err (process-read p :stream 'stderr))
         ) ;
      (process-close p)
      (list code (if (eof-object? out) "" out) (if (eof-object? err) "" err))
    ) ;let
  ) ;let*
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (check-true (number? port))

  ;; 1. 非流式：key1 403 → 自动轮换 key2 成功（未输出实质内容前透明切换）
  (let ((r (run-pi FIXTURE_DIR base-url "multikey-403" #f)))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "RotatedOK:mk-gem-key-2-bbbbbb"))
    (check-true (string-contains? (caddr r)
                  "multikey[google]: 403 on mk1 (mk-gem…aaaa), rotating to another key"))
    ;; 脱敏：stderr 不含完整 key
    (check-true (not (string-contains? (caddr r) "mk-gem-key-1-aaaaaa")))
    (check-true (not (string-contains? (caddr r) "mk-gem-key-2-bbbbbb")))
    (check-true (not (string-contains? (cadr r) "mk-gem-key-1-aaaaaa")))
  ) ;let

  ;; 2. 非流式：key1 429 → 自动轮换 key2 成功
  (let ((r (run-pi FIXTURE_DIR base-url "multikey-429" #f)))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "RotatedOK:mk-gem-key-2-bbbbbb"))
  ) ;let

  ;; 3. 流式（SSE）：key1 403 → 自动轮换 key2 成功
  (let ((r (run-pi FIXTURE_DIR base-url "multikey-stream-403" #f)))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "RotatedOK:mk-gem-key-2-bbbbbb"))
  ) ;let

  ;; 4. 流式（SSE）：key1 429 → 自动轮换 key2 成功
  (let ((r (run-pi FIXTURE_DIR base-url "multikey-stream-429" #f)))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "RotatedOK:mk-gem-key-2-bbbbbb"))
  ) ;let

  ;; 5. pool 存在时优先于 auth.json（PlainOK 回显实际使用的 pool key）
  (let ((r (run-pi FIXTURE_DIR base-url "multikey-plain" #f)))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "PlainOK:mk-gem-key-1-aaaaaa"))
    ;; 首个 key 的正常选择不打印 multikey 日志
    (check-false (string-contains? (caddr r) "multikey["))
  ) ;let

  ;; 5A. 同一会话多轮请求：无异常时始终使用 key-1，不做 LRU 轮询
  (let ((r (run-pi FIXTURE_DIR base-url "multikey-sticky" #f)))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r)
                  "StickyOK:mk-gem-key-1-aaaaaa,mk-gem-key-1-aaaaaa"))
    (check-false (string-contains? (caddr r) "multikey["))
  ) ;let

  ;; 6. pool 耗尽：所有 key 403 → 退出码 1，stderr 报错且脱敏
  (let ((r (run-pi FIXTURE_DIR base-url "multikey-all-403" #f)))
    (check (car r) => 1)
    (check-true (string-contains? (caddr r) "HTTP 403"))
    (check-true (not (string-contains? (caddr r) "mk-gem-key-1-aaaaaa")))
    (check-true (not (string-contains? (caddr r) "mk-gem-key-2-bbbbbb")))
    (check-true (not (string-contains? (cadr r) "mk-gem-key-1-aaaaaa")))
  ) ;let

  ;; 7. 无 pool 时回退 auth.json：使用 auth 中的 key 正常请求
  (let ((r (run-pi FIXTURE_NOPOOL (string-append base-url "/multikey-plain")
             "multikey-plain" #f)))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "PlainOK:plain-google-key-xxxx"))
  ) ;let

  ;; 8. kimi-coding 非流式：key1 403 → 轮换 key2 成功
  (let ((r (run-pi FIXTURE_DIR (string-append base-url "/mk-kimi-403") "kimi-mk"
             "kimi-coding")))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "RotatedOK:mk-kimi-key-2-bbbbbb"))
    (check-true (string-contains? (caddr r)
                  "multikey[kimi-coding]: 403 on mk1 (mk-kim…aaaa), rotating to another key"))
    (check-true (not (string-contains? (caddr r) "mk-kimi-key-1-aaaaaa")))
  ) ;let

  ;; 9. kimi-coding 非流式：key1 429 → 轮换 key2 成功
  (let ((r (run-pi FIXTURE_DIR (string-append base-url "/mk-kimi-429") "kimi-mk"
             "kimi-coding")))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "RotatedOK:mk-kimi-key-2-bbbbbb"))
  ) ;let

  ;; 10. kimi-coding 流式（SSE）：key1 403 → 轮换 key2 成功
  (let ((r (run-pi FIXTURE_DIR (string-append base-url "/mk-kimi-stream-403") "kimi-mk"
             "kimi-coding")))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "RotatedOK:mk-kimi-key-2-bbbbbb"))
  ) ;let

  ;; 11. kimi-coding 流式（SSE）：key1 429 → 轮换 key2 成功
  (let ((r (run-pi FIXTURE_DIR (string-append base-url "/mk-kimi-stream-429") "kimi-mk"
             "kimi-coding")))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "RotatedOK:mk-kimi-key-2-bbbbbb"))
  ) ;let

  ;; 12. kimi-coding pool 耗尽：所有 key 403 → 退出码 1，stderr 脱敏
  (let ((r (run-pi FIXTURE_DIR (string-append base-url "/mk-kimi-all-403") "kimi-mk"
             "kimi-coding")))
    (check (car r) => 1)
    (check-true (string-contains? (caddr r) "HTTP 403"))
    (check-true (not (string-contains? (caddr r) "mk-kimi-key-1-aaaaaa")))
    (check-true (not (string-contains? (caddr r) "mk-kimi-key-2-bbbbbb")))
  ) ;let
    ) ;lambda
    (lambda ()
      (process-kill proc)
      (process-close proc)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
