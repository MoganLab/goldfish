(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme process-context)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii subprocess)
  (liii sys)
  (liii json)
  (liii shutil)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

;; 校验一行 JSONL 输出可被 (liii json) 完整往返：解析→序列化→再解析→再序列化结果稳定
;; （替代此前依赖外部 jq 的交叉验证，避免 CI 环境缺少 jq 导致失败）
(define (json-line-round-trip-ok? line)
  (let* ((value (string->json line))
         (text (json->string value)))
    (equal? text (json->string (string->json text)))
  ) ;let*
) ;define

(define* (read-all-output p (stream 'stdout))
  (let loop ((acc ""))
    (let ((c (process-read p :stream stream)))
      (if (eof-object? c) acc (loop (string-append acc c))))))

(define (split-json-lines text)
  (filter (lambda (s) (not (string-null? s)))
    (map string-trim-both (string-split text "\n"))))

;; 自检：非法 JSON 必须被 (liii json) 拒绝（保证往返校验非空转）
(check-catch 'parse-error (json-line-round-trip-ok? "{\"broken\": }"))

(define stub-server-file
  (if (file-exists? "tests/stub_gemini_server.py")
    "tests/stub_gemini_server.py"
    (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
      "tools/pi/tests/stub_gemini_server.py"
      "../../tools/pi/tests/stub_gemini_server.py"
    ) ;if
  ) ;if
) ;define

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/direct")
    "tests/fixtures/direct"
    (if (file-exists? "tools/pi/tests/fixtures/direct")
      "tools/pi/tests/fixtures/direct"
      "../../tools/pi/tests/fixtures/direct"
    ) ;if
  ) ;if
) ;define

(define GF_BIN (executable))

(define %tmp-agent-dir
  (path->string (path-join (path-temp-dir) "gf-pi-json-e2e-direct"))
)

(define (setup-fixtures!)
  (unless (path-exists? (path %tmp-agent-dir))
    (path-mkdir (path %tmp-agent-dir) :parents #t :exist-ok #t)
  ) ;unless
  (for-each
    (lambda (name)
      (let ((src (path-join (path FIXTURE_DIR) (path name)))
            (dst (path-join (path %tmp-agent-dir) (path name)))
           ) ;
        (when (path-exists? src)
          (path-copy src dst)
        ) ;when
      ) ;let
    ) ;lambda
    '("auth.json" "models-store.json" "models.json" "settings.json")
  ) ;for-each
) ;define

(define (cleanup-fixtures!)
  (when (path-exists? (path %tmp-agent-dir))
    (rmtree %tmp-agent-dir #t)
  ) ;when
) ;define

(setup-fixtures!)

(dynamic-wind
  (lambda () #f)
  (lambda ()
    (let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
           (port-line (process-read-line proc))
           (port (parse-port-line port-line))
           (base-url (string-append "http://127.0.0.1:" (number->string port)))
           (test-env (append (get-environment-variables)
                       (list (cons "PI_CODING_AGENT_DIR" %tmp-agent-dir)
                         (cons "GF_PI_BASE_URL" base-url)
                         (cons "GF_PI_MODEL" "gemini-3.8-flash")
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
          ) ;
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (check-true (number? port))

  ;; 1. 单轮简单对话的 JSONL 事件序列
  (let* ((cmd (list GF_BIN "pi" "--mode" "json" "Hello from json e2e test"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (raw-out (read-all-output p))
         (code (process-wait p 10000))
         (lines (split-json-lines raw-out))
         (events (map string->json lines))
         (event-types
           (map (lambda (ev) (cdr (assoc "type" ev))) events)
         ) ;event-types
        ) ;
    (check code => 0)
    (check-true (>= (length lines) 8))
    ;; 第一条记录必须是 session header (version 3)
    (let ((header (car events)))
      (check (cdr (assoc "type" header)) => "session")
      (check (cdr (assoc "version" header)) => 3)
      (check-true (string? (cdr (assoc "id" header))))
      (check-true (string? (cdr (assoc "cwd" header))))
    ) ;let
    ;; 验证基础运行的事件类型序列（流式模式下包含 message_update）
    (check-true (pair? (member "message_update" event-types)))
    (check (car event-types) => "session")
    (check (cadr event-types) => "agent_start")
    (check (caddr event-types) => "turn_start")
    (check (car (reverse event-types)) => "agent_settled")
    (process-close p)
  ) ;let*

  ;; 2. 多轮带工具调用（functionCall）的 JSONL 事件序列
  (let* ((cmd (list GF_BIN "pi" "--mode" "json" "--model" "multi-turn" "Run multi turn steps")
         ) ;cmd
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (raw-out (read-all-output p))
         (code (process-wait p 15000))
         (lines (split-json-lines raw-out))
         (events (map string->json lines))
         (event-types
           (map (lambda (ev) (cdr (assoc "type" ev))) events)
         ) ;event-types
        ) ;
    (check code => 0)
    ;; 验证每行 JSON 都能被 (liii json) 完整往返（解析→序列化→再解析→再序列化结果稳定）
    (check-true (every json-line-round-trip-ok? lines))
    ;; 检查关键事件包含 tool_execution_start, tool_execution_end
    (check-true (pair? (member "tool_execution_start" event-types)))
    (check-true (pair? (member "tool_execution_end" event-types)))
    (check (car event-types) => "session")
    (check (car (reverse event-types)) => "agent_settled")
    (process-close p)
  ) ;let*

  ;; 3. 模型报错/aborted 情况下进程退出码仍为 0（§3.3：事件流中出现 error/aborted 不改变退出码）
  (let* ((cmd (list GF_BIN "pi" "--mode" "json" "--model" "safety" "Run safety check"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (raw-out (read-all-output p))
         (code (process-wait p 10000))
         (lines (split-json-lines raw-out))
         (events (map string->json lines))
         (event-types
           (map (lambda (ev) (cdr (assoc "type" ev))) events)
         ) ;event-types
        ) ;
    (check code => 0)
    (check-true (pair? (member "session" event-types)))
    (check-true (pair? (member "agent_settled" event-types)))
    (process-close p)
  ) ;let*

  ;; 4. minimax-cn provider JSON 模式事件流验证（anthropic-messages 协议）
  ;; GF_PI_MODEL 显式指定模型，避免与 provider 不匹配（任务 034 严格校验）
  (let* ((ant-env (append test-env
                    (list (cons "MINIMAX_CN_API_KEY" "sk-ant-test-json")
                      (cons "MINIMAX_CN_BASE_URL" base-url)
                      (cons "GF_PI_MODEL" "MiniMax-M2.7")
                    ) ;list
                  ) ;append
         ) ;ant-env
         (cmd (list GF_BIN "pi" "--provider" "minimax-cn" "--model"
                "MiniMax-M2.7" "--mode" "json" "Hello anthropic json"
              ) ;list
         ) ;cmd
         (p (process-spawn cmd :env ant-env :stdout 'pipe :stderr 'pipe))
         (raw-out (read-all-output p))
         (code (process-wait p 10000))
         (lines (split-json-lines raw-out))
         (events (map string->json lines))
         (event-types
           (map (lambda (ev) (cdr (assoc "type" ev))) events)
         ) ;event-types
        ) ;
    (check code => 0)
    (check (car event-types) => "session")
    (check (car (reverse event-types)) => "agent_settled")
    (check-true (pair? (member "message_update" event-types)))
    (process-close p)
  ) ;let*

  ;; 5. 缺陷8防回归测试：4字节 UTF-8 字符（emoji）与 '<' 组合在 JSON 模式事件流验证
  (let* ((cmd (list GF_BIN "pi" "--mode" "json" "🚀 <"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (raw-out (read-all-output p))
         (code (process-wait p 10000))
         (lines (split-json-lines raw-out))
         (events (map string->json lines))
         (event-types
           (map (lambda (ev) (cdr (assoc "type" ev))) events)
         ) ;event-types
        ) ;
    (check code => 0)
    (check (car event-types) => "session")
    (check (car (reverse event-types)) => "agent_settled")
    (process-close p)
  ) ;let*
        ) ;lambda
        (lambda ()
          (process-kill proc)
          (process-close proc)
        ) ;lambda
      ) ;dynamic-wind
    ) ;let*
  ) ;lambda
  (lambda ()
    (cleanup-fixtures!)
  ) ;lambda
) ;dynamic-wind

(check-report)
