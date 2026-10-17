(import (liii check)
        (liii string)
        (liii path)
        (liii pi))

(check-set-mode! 'report-failed)

;; resolve-app-mode
(check (resolve-app-mode "json" #f #t #t) => 'json)
(check (resolve-app-mode 'json #f #t #t) => 'json)
(check (resolve-app-mode "text" #f #t #t) => 'print)
(check (resolve-app-mode #f #t #t #t) => 'print)
(check (resolve-app-mode #f #f #f #t) => 'print)
(check (resolve-app-mode #f #f #t #f) => 'print)
(check (resolve-app-mode #f #f #t #t) => 'print)

;; format-file-block
(check (format-file-block "/path/to/foo.txt" "hello world")
  => "<file name=\"/path/to/foo.txt\">\nhello world\n</file>\n")
(check (format-file-block "/path/to/bar.txt" "line 1\nline 2")
  => "<file name=\"/path/to/bar.txt\">\nline 1\nline 2\n</file>\n")

;; build-initial-prompt:
;; parts = [stdin?, file-text?, first-message] 无分隔符直接 join
;; 1. 只有 message
(let-values (((prompt rest) (build-initial-prompt #f "" '("message 1" "message 2"))))
  (check prompt => "message 1")
  (check rest => '("message 2")))

;; 2. stdin + message
(let-values (((prompt rest) (build-initial-prompt "stdin content\n" "" '("first message"))))
  (check prompt => "stdin content\nfirst message")
  (check rest => '()))

;; 3. stdin + file + message
(let-values (((prompt rest)
              (build-initial-prompt "stdin content\n"
                                   "<file name=\"/foo\">bar</file>\n"
                                   '("do something" "followup"))))
  (check prompt => "stdin content\n<file name=\"/foo\">bar</file>\ndo something")
  (check rest => '("followup")))

;; 4. 只有 file-text，无 messages
(let-values (((prompt rest)
              (build-initial-prompt #f "<file name=\"/foo\">bar</file>\n" '())))
  (check prompt => "<file name=\"/foo\">bar</file>\n")
  (check rest => '()))

;; 5. 全部为空
(let-values (((prompt rest) (build-initial-prompt #f "" '())))
  (check prompt => "")
  (check rest => '()))

;; 截断常量
(check PI_DEFAULT_MAX_LINES => 2000)
(check PI_DEFAULT_MAX_BYTES => 51200)
(check PI_GREP_MAX_LINE_LENGTH => 500)

;; pi-format-size
(check (pi-format-size 500) => "500B")
(check (pi-format-size 1024) => "1.0KB")
(check (pi-format-size 1536) => "1.5KB")
(check (pi-format-size 51200) => "50.0KB")
(check (pi-format-size (* 1024 1024)) => "1.0MB")

;; pi-truncate-line
(let-values (((line truncated?) (pi-truncate-line "hello world")))
  (check line => "hello world")
  (check truncated? => #f))

(let* ((long-line (make-string 600 #\a)))
  (let-values (((line truncated?) (pi-truncate-line long-line)))
    (check (string-length line) => 500)
    (check truncated? => #t)))

;; pi-truncate-head: 未超出限制
(let ((res (pi-truncate-head "line1\nline2\nline3" 10 1000)))
  (check (cdr (assoc 'truncated res)) => #f)
  (check (cdr (assoc 'content res)) => "line1\nline2\nline3")
  (check (cdr (assoc 'output-lines res)) => 3)
  (check (cdr (assoc 'total-lines res)) => 3))

;; pi-truncate-head: 按行截断
(let ((res (pi-truncate-head "line1\nline2\nline3\nline4" 2 1000)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'lines)
  (check (cdr (assoc 'content res)) => "line1\nline2")
  (check (cdr (assoc 'output-lines res)) => 2)
  (check (cdr (assoc 'total-lines res)) => 4))

;; pi-truncate-head: 按字节截断
(let ((res (pi-truncate-head "12345\n67890\nabcde" 10 12)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  ;; "12345" (5) + \n (1) + "67890" (5) = 11 <= 12; 第三行 +1+5=17 > 12
  (check (cdr (assoc 'content res)) => "12345\n67890")
  (check (cdr (assoc 'output-lines res)) => 2))

;; pi-truncate-head: 第一行单行超字节限
(let ((res (pi-truncate-head "1234567890\nline2" 10 5)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'first-line-exceeds-limit? res)) => #t)
  (check (cdr (assoc 'content res)) => ""))

;; pi-truncate-tail: 未超限
(let ((res (pi-truncate-tail "line1\nline2\nline3" 10 1000)))
  (check (cdr (assoc 'truncated res)) => #f)
  (check (cdr (assoc 'content res)) => "line1\nline2\nline3"))

;; pi-truncate-tail: 按行截断（保留尾部行）
(let ((res (pi-truncate-tail "line1\nline2\nline3\nline4" 2 1000)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'lines)
  (check (cdr (assoc 'content res)) => "line3\nline4")
  (check (cdr (assoc 'output-lines res)) => 2))

;; pi-truncate-tail: 按字节截断（保留尾部）
(let ((res (pi-truncate-tail "12345\n67890\nabcde" 10 12)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'content res)) => "67890\nabcde"))

;; Tool smoke tests (detailed suite in tools/pi/tests/liii/goldpi-tools-test.scm)
(check-true (string? (pi-execute-read '(("path" . "tests/goldfish/liii/pi-test.scm")))))
(let ((out (pi-execute-ls '(("path" . "tests/goldfish")))))
  (check-true (string-contains? out "liii/")))
(let-values (((out is-err) (pi-execute-bash '(("command" . "echo 'ok'")))))
  (check is-err => #f))

;; pi-resolve-config-value
(check (pi-resolve-config-value "literal-key") => "literal-key")
(let ((test-env '(("TEST_LEFT" . "left") ("TEST_RIGHT" . "right"))))
  (check (pi-resolve-config-value "$TEST_LEFT" test-env) => "left")
  (check (pi-resolve-config-value "${TEST_LEFT}_$TEST_RIGHT" test-env) => "left_right")
  (check (pi-resolve-config-value "$$TEST_LEFT" test-env) => "$TEST_LEFT")
  (check (pi-resolve-config-value "$!literal-$TEST_RIGHT" test-env) => "!literal-right")
  (check (pi-resolve-config-value "$UNKNOWN_ENV_VAR_123" test-env) => #f)
  (check (pi-resolve-config-value "${UNKNOWN_ENV_VAR_123}" test-env) => #f))

(check-catch 'pi-config-error (pi-resolve-config-value "!echo 'not-allowed'"))
(check-catch 'type-error (pi-resolve-config-value 123))

;; json-dict-ref
(let ((j '(("foo" . 1) (bar . 2))))
  (check (json-dict-ref j "foo" 0) => 1)
  (check (json-dict-ref j 'foo 0) => 1)
  (check (json-dict-ref j "bar" 0) => 2)
  (check (json-dict-ref j 'bar 0) => 2)
  (check (json-dict-ref j "baz" 99) => 99))

;; pi-get-agent-dir
(check-true (string? (pi-get-agent-dir)))
(check-true (string-ends? (pi-get-agent-dir) "/.pi/agent"))
(check-true (string-ends? (pi-get-agent-dir "tools/pi/tests/fixtures/default")
                          "/tools/pi/tests/fixtures/default"))

;; pi-read-json-file
(check (pi-read-json-file "non_existent_file.json") => #f)
(let ((data (pi-read-json-file "tools/pi/tests/fixtures/default/settings.json")))
  (check-true (pair? data)))

;; pi-load-settings
(let ((settings (pi-load-settings "tools/pi/tests/fixtures/default")))
  (check (cdr (assoc 'default-provider settings)) => "google")
  (check (cdr (assoc 'default-model settings)) => "gemini-3.8-flash")
  (check (cdr (assoc 'http-proxy settings)) => "http://127.0.0.1:7890")
  (let ((retry (cdr (assoc 'retry settings))))
    (check (cdr (assoc 'enabled retry)) => #t)
    (check (cdr (assoc 'max-retries retry)) => 3)
    (check (cdr (assoc 'base-delay-ms retry)) => 2000)
    (check (cdr (assoc 'max-agent-delay-ms retry)) => 60000))
  (check (cdr (assoc 'default-tools settings)) => '("read" "bash" "edit" "write")))

;; pi-load-settings on empty/non-existent dir falls back to defaults
(let ((empty-settings (pi-load-settings "non/existent/dir")))
  (check (cdr (assoc 'default-provider empty-settings)) => "google")
  (check (cdr (assoc 'default-model empty-settings)) => "gemini-3.8-flash")
  (check (cdr (assoc 'http-proxy empty-settings)) => #f)
  (check (cdr (assoc 'default-tools empty-settings)) => #f))

;; pi-resolve-proxy-url
(let ((settings '((http-proxy . "http://127.0.0.1:7890")))
      (clean-env '())
      (env-with-https '(("HTTPS_PROXY" . "https://corp-proxy:8080")))
      (env-with-all '(("ALL_PROXY" . "socks5h://127.0.0.1:1080"))))
  ;; CLI 选项优先
  (check (pi-resolve-proxy-url "socks5h://127.0.0.1:1080" settings clean-env)
    => "socks5h://127.0.0.1:1080")
  ;; HTTPS_PROXY 优先于 settings
  (check (pi-resolve-proxy-url #f settings env-with-https)
    => "https://corp-proxy:8080")
  ;; ALL_PROXY 优先于 settings
  (check (pi-resolve-proxy-url #f settings env-with-all)
    => "socks5h://127.0.0.1:1080")
  ;; 降级到 settings.httpProxy
  (check (pi-resolve-proxy-url #f settings clean-env)
    => "http://127.0.0.1:7890")
  (check (pi-resolve-proxy-url "" settings clean-env)
    => "http://127.0.0.1:7890")
  ;; 无代理直连
  (check (pi-resolve-proxy-url #f '((http-proxy . #f)) clean-env)
    => #f))

;; pi-load-auth
(let ((auth (pi-load-auth "tools/pi/tests/fixtures/default"
                          '(("TEST_ANTHROPIC_KEY" . "sk-ant-test-12345")))))
  (check (cdr (assoc "google" auth)) => "test-google-key-from-auth-json")
  (check (cdr (assoc "anthropic" auth)) => "sk-ant-test-12345")
  (check (assoc "kimi-coding" auth) => #f))

(check-catch 'pi-config-error
  (pi-load-auth "tools/pi/tests/fixtures/invalid_cmd"))

;; pi-resolve-provider-key
(let ((auth-with-google '(("google" . "key-from-auth")))
      (auth-empty '())
      (env-with-gemini '(("GEMINI_API_KEY" . "gemini-env-key-999"))))
  ;; 存储条目优先
  (check (pi-resolve-provider-key "google" auth-with-google env-with-gemini)
    => "key-from-auth")
  ;; 存储条目不存在时，探测环境变量 GEMINI_API_KEY
  (check (pi-resolve-provider-key "google" auth-empty env-with-gemini)
    => "gemini-env-key-999")
  ;; 均未配置时返回 #f
  (check (pi-resolve-provider-key "google" auth-empty '())
    => #f)
  (check (pi-resolve-provider-key "unknown-provider" auth-empty '())
    => #f))

(check-report)
