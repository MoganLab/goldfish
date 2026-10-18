(import (scheme base)
  (scheme file)
  (liii check)
  (liii subprocess)
  (liii json)
  (liii string)
  (liii list)
  (liii os)
  (liii path)
) ;import

(check-set-mode! 'report-failed)

;; [006] record 化·配置结构 验收脚本
(set! *load-path* (cons "tools/pi" *load-path*))
(eval '(import (liii pi)) (rootlet))

;; 断言点 1: settings 解析结果/凭据/模型元数据为 define-record-type（谓词与访问器可用）

;; 1.1 settings & retry-config record 构造器、谓词与访问器
(let* ((test-retry (pi-retry-config #t 5 1500 45000))
       (tools-val '("read" "bash" "edit" "write"))
       (s (pi-settings "google" "gemini-3.8-flash" "medium" "http://proxy:8080"
            test-retry tools-val
          ) ;pi-settings
       ) ;s
      ) ;
  ;; retry-config
  (check-true (pi-retry-config? test-retry))
  (check (pi-retry-config-enabled test-retry) => #t)
  (check (pi-retry-config-max-retries test-retry) => 5)
  (check (pi-retry-config-base-delay-ms test-retry) => 1500)
  (check (pi-retry-config-max-agent-delay-ms test-retry) => 45000)

  ;; settings
  (check-true (pi-settings? s))
  (check-false (pi-credential? s))
  (check-false (pi-model-metadata? s))
  (check (pi-settings-default-provider s) => "google")
  (check (pi-settings-default-model s) => "gemini-3.8-flash")
  (check (pi-settings-default-thinking-level s) => "medium")
  (check (pi-settings-http-proxy s) => "http://proxy:8080")
  (check (pi-settings-retry s) => test-retry)
  (check (pi-settings-default-tools s) => tools-val)
) ;let*

;; 1.2 credential record 构造器、谓词与访问器
(let ((c (pi-credential "google" "test-api-key" "api_key")))
  (check-true (pi-credential? c))
  (check-false (pi-settings? c))
  (check-false (pi-model-metadata? c))
  (check (pi-credential-provider c) => "google")
  (check (pi-credential-key c) => "test-api-key")
  (check (pi-credential-type c) => "api_key")
) ;let

;; 1.3 model-metadata record 构造器、谓词与访问器
(let ((m (pi-model-metadata "gemini-3.8-flash" "Gemini 3.8 Flash" "google"
           "https://generativelanguage.googleapis.com" 1048576 8192 #t
         ) ;pi-model-metadata
      ) ;m
     ) ;
  (check-true (pi-model-metadata? m))
  (check-false (pi-settings? m))
  (check-false (pi-credential? m))
  (check (pi-model-metadata-id m) => "gemini-3.8-flash")
  (check (pi-model-metadata-name m) => "Gemini 3.8 Flash")
  (check (pi-model-metadata-provider m) => "google")
  (check (pi-model-metadata-base-url m)
    =>
    "https://generativelanguage.googleapis.com"
  ) ;check
  (check (pi-model-metadata-context-window m) => 1048576)
  (check (pi-model-metadata-max-tokens m) => 8192)
  (check (pi-model-metadata-matched? m) => #t)
) ;let

;; 1.4 加载函数返回 record 验证
(let* ((settings (pi-load-settings "tools/pi/tests/fixtures/default"))
       (auth-list (pi-load-auth "tools/pi/tests/fixtures/default"))
       (models-store (pi-load-models-store "tools/pi/tests/fixtures/default"))
       (meta (pi-resolve-model-metadata "google" "gemini-2.5-flash" models-store))
      ) ;
  (check-true (pi-settings? settings))
  (check-true (pi-credential? (car auth-list)))
  (check (cdr (assoc "google" auth-list)) => "test-google-key-from-auth-json")
  (check-true (pi-model-metadata? meta))
) ;let*

;; 断言点 2: assoc 兼容访问不回归（含 string 与 symbol 键）
(let* ((tools-val '("read" "bash"))
       (test-retry (pi-retry-config #t 3 2000 60000))
       (s (pi-settings "google" "gemini-3.8-flash" "off" #f test-retry tools-val))
       (c (pi-credential "kimi-coding" "sk-kimi-123" "api_key"))
       (m (pi-model-metadata "kimi-k1.5" "Kimi K1.5" "kimi-coding"
            "https://api.moonshot.cn" 128000 4096 #f
          ) ;pi-model-metadata
       ) ;m
      ) ;
  ;; settings assoc 兼容（string 与 symbol 键）
  (check (cdr (assoc 'default-provider s)) => "google")
  (check (cdr (assoc "default-provider" s)) => "google")
  (check (cdr (assoc 'default-model s)) => "gemini-3.8-flash")
  (check (cdr (assoc "default-model" s)) => "gemini-3.8-flash")
  (check (cdr (assoc 'default-thinking-level s)) => "off")
  (check (cdr (assoc "default-thinking-level" s)) => "off")
  (check (cdr (assoc 'http-proxy s)) => #f)
  (check (cdr (assoc "http-proxy" s)) => #f)
  (check (cdr (assoc 'retry s)) => test-retry)
  (check (cdr (assoc "retry" s)) => test-retry)
  (check (cdr (assoc 'default-tools s)) => tools-val)
  (check (cdr (assoc "default-tools" s)) => tools-val)

  ;; retry assoc 兼容（string 与 symbol 键）
  (check (cdr (assoc 'enabled test-retry)) => #t)
  (check (cdr (assoc "enabled" test-retry)) => #t)
  (check (cdr (assoc 'max-retries test-retry)) => 3)
  (check (cdr (assoc "max-retries" test-retry)) => 3)
  (check (cdr (assoc 'base-delay-ms test-retry)) => 2000)
  (check (cdr (assoc "base-delay-ms" test-retry)) => 2000)
  (check (cdr (assoc 'max-agent-delay-ms test-retry)) => 60000)
  (check (cdr (assoc "max-agent-delay-ms" test-retry)) => 60000)

  ;; credential assoc 兼容（string 与 symbol 键）
  (check (cdr (assoc 'provider c)) => "kimi-coding")
  (check (cdr (assoc "provider" c)) => "kimi-coding")
  (check (cdr (assoc 'key c)) => "sk-kimi-123")
  (check (cdr (assoc "key" c)) => "sk-kimi-123")
  (check (cdr (assoc 'type c)) => "api_key")
  (check (cdr (assoc "type" c)) => "api_key")

  ;; model-metadata assoc 兼容（string 与 symbol 键）
  (check (cdr (assoc 'id m)) => "kimi-k1.5")
  (check (cdr (assoc "id" m)) => "kimi-k1.5")
  (check (cdr (assoc 'name m)) => "Kimi K1.5")
  (check (cdr (assoc "name" m)) => "Kimi K1.5")
  (check (cdr (assoc 'provider m)) => "kimi-coding")
  (check (cdr (assoc "provider" m)) => "kimi-coding")
  (check (cdr (assoc 'base-url m)) => "https://api.moonshot.cn")
  (check (cdr (assoc "base-url" m)) => "https://api.moonshot.cn")
  (check (cdr (assoc 'context-window m)) => 128000)
  (check (cdr (assoc "context-window" m)) => 128000)
  (check (cdr (assoc 'max-tokens m)) => 4096)
  (check (cdr (assoc "max-tokens" m)) => 4096)
  (check (cdr (assoc 'matched? m)) => #f)
  (check (cdr (assoc "matched?" m)) => #f)
) ;let*

;; 断言点 3: JSON 输出与 CLI 行为零变化
;; 3.1 bin/gf pi -p 冒烟验证
(let* ((tmp-out "/tmp/pi-006-smoke-print.txt")
       (ret (run "sh -c 'bin/gf pi -p --no-session \"1+1 等于几？\" > /tmp/pi-006-smoke-print.txt 2>/dev/null'"
              :timeout 60
            ) ;run
       ) ;ret
       (out
         (if (file-exists? tmp-out)
           (call-with-input-file tmp-out
             (lambda (p)
               (let loop
                 ((acc '()))
                 (let ((l (read-line p)))
                   (if (eof-object? l) (string-join (reverse acc) "\n") (loop (cons l acc)))
                 ) ;let
               ) ;let
             ) ;lambda
           ) ;call-with-input-file
           ""
         ) ;if
       ) ;out
      ) ;
  (check ret => 0)
  (check-true (> (string-length (string-trim out)) 0))
  (if (file-exists? tmp-out) (delete-file tmp-out))
) ;let*

;; 3.2 bin/gf pi --mode json 冒烟验证（校验 JSON 事件流完整性与事件键值）
(let* ((tmp-out "/tmp/pi-006-smoke-json.jsonl")
       (ret (run "sh -c 'bin/gf pi --mode json --no-session --max-turns 1 \"1+1 等于几？\" > /tmp/pi-006-smoke-json.jsonl 2>/dev/null'"
              :timeout 60
            ) ;run
       ) ;ret
       (lines
         (if (file-exists? tmp-out)
           (call-with-input-file tmp-out
             (lambda (p)
               (let loop
                 ((acc '()))
                 (let ((l (read-line p)))
                   (if (eof-object? l) (reverse acc) (loop (cons l acc)))
                 ) ;let
               ) ;let
             ) ;lambda
           ) ;call-with-input-file
           '()
         ) ;if
       ) ;lines
       (json-types
         (let loop
           ((ls lines) (acc '()))
           (if (null? ls)
             (reverse acc)
             (let* ((line (car ls))
                    (t
                      (guard (e (else #f))
                        (let ((j (string->json line)))
                          (and (pair? j)
                            (cond ((assoc "type" j) => cdr)
                                  ((assoc 'type j) => cdr)
                                  (else #f)
                            ) ;cond
                          ) ;and
                        ) ;let
                      ) ;guard
                    ) ;t
                   ) ;
               (if t (loop (cdr ls) (cons t acc)) (loop (cdr ls) acc))
             ) ;let*
           ) ;if
         ) ;let
       ) ;json-types
      ) ;
  (check ret => 0)
  (for-each
    (lambda (type-name)
      (check-true (and (member type-name json-types string=?) #t))
    ) ;lambda
    (list "session" "agent_start" "turn_start" "message_start" "message_end"
      "turn_end" "agent_end" "agent_settled"
    ) ;list
  ) ;for-each
  (if (file-exists? tmp-out) (delete-file tmp-out))
) ;let*

;; 断言点 4: bin/gf test 全量测试过（含 tools/pi/tests 目录）
(check (run "sh -c 'for f in tools/pi/tests/liii/*-test.scm tools/pi/tests/liii/pi/*-test.scm; do bin/gf test \"$f\" >/dev/null 2>&1 || exit 1; done'"
         :timeout 300
       ) ;run
  =>
  0
) ;check

;; 断言点 5: bin/gf pi --help 与 doc 抽查可用
(check (run "bin/gf pi --help >/dev/null 2>&1" :timeout 30) => 0)
(check (run "bin/gf doc substring >/dev/null 2>&1" :timeout 30) => 0)

(check-report)
