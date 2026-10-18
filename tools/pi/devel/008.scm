(import (scheme base)
  (scheme file)
  (scheme read)
  (liii check)
  (liii subprocess)
  (liii json)
  (liii string)
  (liii list)
  (liii os)
  (liii path)
) ;import

(check-set-mode! 'report-failed)

;; [008] Kimi provider 完整支持 验收脚本
(set! *load-path* (cons "tools/pi" *load-path*))
(eval '(import (liii pi)) (rootlet))
(eval '(import (liii pi kimi)) (rootlet))
(eval '(import (liii pi kimi core)) (rootlet))
(eval '(import (liii pi kimi msg)) (rootlet))
(eval '(import (liii pi kimi request)) (rootlet))
(eval '(import (liii pi kimi stream-parse)) (rootlet))
(eval '(import (liii pi kimi stream)) (rootlet))

(define (%count-file-lines path)
  (call-with-input-file path
    (lambda (p)
      (let loop
        ((cnt 0))
        (let ((l (read-line p)))
          (if (eof-object? l) cnt (loop (+ cnt 1)))
        ) ;let
      ) ;let
    ) ;lambda
  ) ;call-with-input-file
) ;define

(define (%read-file-content path)
  (call-with-input-file path
    (lambda (p)
      (let loop
        ((acc '()))
        (let ((l (read-line p)))
          (if (eof-object? l) (string-join (reverse acc) "\n") (loop (cons l acc)))
        ) ;let
      ) ;let
    ) ;lambda
  ) ;call-with-input-file
) ;define

;; ============================================================================
;; 验收点 1: (liii pi kimi) 子库存在且可 import，各子模块正常导出
;; ============================================================================
(check-true (procedure? kimi-map-stop-reason))
(check-true (procedure? kimi-map-usage))
(check-true (procedure? kimi-parse-response))
(check-true (procedure? kimi-is-retriable-error?))
(check-true (procedure? kimi-build-tool-declarations))
(check-true (procedure? kimi-build-messages))
(check-true (procedure? kimi-build-request-body))
(check-true (procedure? make-kimi-stream-decoder))
(check-true (procedure? kimi-build-endpoint))
(check-true (procedure? kimi-build-tool-declarations-from-names))
(check-true (procedure? kimi-request))
(check-true (procedure? kimi-request-stream))

;; ============================================================================
;; 验收点 2: Kimi 谓词/访问器与映射函数单测
;; ============================================================================

;; 2.1 kimi-map-stop-reason
(check (kimi-map-stop-reason "stop") => "stop")
(check (kimi-map-stop-reason "length") => "length")
(check (kimi-map-stop-reason "tool_calls") => "toolUse")
(check (kimi-map-stop-reason 'tool_calls) => "toolUse")
(check (kimi-map-stop-reason "content_filter") => "error")
(check (kimi-map-stop-reason "unknown_reason") => "error")

;; 2.2 kimi-map-usage
(let ((u (kimi-map-usage '(("prompt_tokens" . 10)
                           ("completion_tokens" . 20)
                           ("total_tokens" . 30)
                           ("cached_tokens" . 5))
         ) ;kimi-map-usage
      ) ;u
     ) ;
  (check (cdr (assoc 'input u)) => 10)
  (check (cdr (assoc 'output u)) => 20)
  (check (cdr (assoc 'cacheRead u)) => 5)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 30)
) ;let

;; 2.3 kimi-is-retriable-error?
(check-true (kimi-is-retriable-error? 429 "rate limit"))
(check-true (kimi-is-retriable-error? 500 "internal error"))
(check-true (kimi-is-retriable-error? 502 "bad gateway"))
(check-true (kimi-is-retriable-error? 503 "service unavailable"))
(check-true (kimi-is-retriable-error? 504 "gateway timeout"))
(check-true (kimi-is-retriable-error? 408 "request timeout"))
(check-false (kimi-is-retriable-error? 200 "ok"))
(check-false (kimi-is-retriable-error? 400 "bad request"))
(check-false (kimi-is-retriable-error? 401 "unauthorized"))
(check-false (kimi-is-retriable-error? 404 "not found"))

;; 2.4 kimi-parse-response 正常文本与 tool_calls 解析
(let* ((resp-json '(("choices"
                     . #((("finish_reason" . "stop")
                          ("message"
                           ("content" . "Hello from Kimi")
                           ("role" . "assistant")))
                       ) ;#
                    )
                    ("usage" ("prompt_tokens" . 5) ("completion_tokens" . 3)))
       ) ;resp-json
       (msg (kimi-parse-response resp-json 200))
      ) ;
  (check (pi-assistant-message-stop-reason msg) => "stop")
  (let ((blocks (pi-assistant-message-content msg)))
    (check (length blocks) => 1)
    (check (pi-text-block-text (car blocks)) => "Hello from Kimi")
  ) ;let
) ;let*

(let* ((tc-json '(("choices"
                   . #((("finish_reason" . "tool_calls")
                        ("message"
                         ("content" . "")
                         ("role" . "assistant")
                         ("tool_calls"
                          . #((("id" . "call_123")
                               ("type" . "function")
                               ("function"
                                ("name" . "bash")
                                ("arguments" . "{\"command\":\"ls\"}")))
                            ) ;#
                         )))
                     ) ;#
                  ))
       ) ;tc-json
       (msg (kimi-parse-response tc-json 200))
      ) ;
  (check (pi-assistant-message-stop-reason msg) => "toolUse")
  (let ((blocks (pi-assistant-message-content msg)))
    (check (length blocks) => 1)
    (let ((tc (car blocks)))
      (check (pi-tool-call-block-id tc) => "call_123")
      (check (pi-tool-call-block-name tc) => "bash")
      (check (cdr (assoc "command" (pi-tool-call-block-arguments tc))) => "ls")
    ) ;let
  ) ;let
) ;let*

;; 2.5 make-kimi-stream-decoder 流式解码与 tool_calls 增量拼装
(let* ((events '())
       (on-ev (lambda (ev) (set! events (cons ev events))))
       (stream-state (vector '() #f '() "stop" #f '()))
       (decoder (make-kimi-stream-decoder stream-state on-ev))
      ) ;
  ;; 传入 delta 文本
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello Kimi\"}}]}\n\n"
  ) ;decoder
  (check (> (length events) 0) => #t)
  (let ((rev-ev (reverse events)))
    (check (cdr (assoc 'type (car rev-ev))) => "text_start")
    (check (cdr (assoc 'delta (cadr rev-ev))) => "Hello Kimi")
  ) ;let
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\",\"usage\":{\"prompt_tokens\":8,\"completion_tokens\":2,\"total_tokens\":10}}]}\n\n"
  ) ;decoder
  (decoder "data: [DONE]\n\n")
  (let ((blocks (vector-ref stream-state 0)))
    (check (length blocks) => 1)
    (check (pi-text-block-text (car blocks)) => "Hello Kimi")
  ) ;let
  (check (vector-ref stream-state 3) => "stop")
  (check (cdr (assoc 'totalTokens (vector-ref stream-state 2))) => 10)
) ;let*

;; 2.6 凭据解析支持 kimi-coding 且环境变量 KIMI_API_KEY 可兜底
(let* ((env-with-key '(("KIMI_API_KEY" . "sk-test-env-key-999")))
       (resolved-key (pi-resolve-provider-key "kimi-coding" '() env-with-key))
      ) ;
  (check resolved-key => "sk-test-env-key-999")
) ;let*

;; 2.7 模型元数据支持 kimi-coding 默认模型与 baseUrl
(let ((meta (pi-resolve-model-metadata "kimi-coding" "kimi-for-coding" '())))
  (check (pi-model-metadata-id meta) => "kimi-for-coding")
  (check (pi-model-metadata-provider meta) => "kimi-coding")
  (check (pi-model-metadata-base-url meta) => "https://api.kimi.com/coding/v1")
  (check (pi-model-metadata-context-window meta) => 262144)
  (check (pi-model-metadata-max-tokens meta) => 8192)
) ;let

;; ============================================================================
;; 验收点 3: bin/gf pi --help 输出包含 kimi
;; ============================================================================
(let* ((tmp-help "/tmp/pi-008-help.txt")
       (ret (run "sh -c 'bin/gf pi --help > /tmp/pi-008-help.txt'" :timeout 30))
      ) ;
  (check ret => 0)
  (let ((content (%read-file-content tmp-help)))
    (check (string-contains content "kimi") => #t)
    (check (string-contains content "kimi-coding") => #t)
  ) ;let
  (if (file-exists? tmp-help) (delete-file tmp-help))
) ;let*

;; ============================================================================
;; 验收点 4: 单文件不超 300 行（硬约束）
;; ============================================================================
(let ((files '("tools/pi/liii/pi/kimi.scm" "tools/pi/liii/pi/kimi/core.scm"
               "tools/pi/liii/pi/kimi/msg.scm"
               "tools/pi/liii/pi/kimi/request.scm"
               "tools/pi/liii/pi/kimi/stream.scm"
               "tools/pi/liii/pi/kimi/stream-parse.scm"
               "tools/pi/liii/pi/agent.scm" "tools/pi/liii/pi/auth.scm"
               "tools/pi/liii/pi/config.scm" "tools/pi/liii/pi/models.scm"
               "tools/pi/liii/pi.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (let ((line-count (%count-file-lines f)))
        (check (< line-count 300) => #t)
      ) ;let
    ) ;lambda
    files
  ) ;for-each
) ;let

;; ============================================================================
;; 验收点 5: bin/gf test 全量过
;; ============================================================================
(let* ((ret (run "bin/gf test tools/pi/tests/liii/" :timeout 180)))
  (check ret => 0)
) ;let*

;; ============================================================================
;; 验收点 6: 错误路径与真实冒烟验证
;; ============================================================================
;; 6.1 凭据缺失时明确报错退出码 1
(let* ((ret (run "sh -c 'PI_CODING_AGENT_DIR=/tmp/nonexistent-pi-agent KIMI_API_KEY= bin/gf pi --provider kimi-coding -p \"hello\" >/dev/null 2>&1'"
              :timeout 30
            ) ;run
       ) ;ret
      ) ;
  (check ret => 1)
) ;let*

;; 6.2 真实网络冒烟验证（纯文本与 bash 工具往返）
(let* ((ret-text (run "sh -c 'bin/gf pi --provider kimi-coding -p \"请回复 PONG\" >/dev/null 2>&1'"
                   :timeout 60
                 ) ;run
       ) ;ret-text
      ) ;
  (check ret-text => 0)
) ;let*

(let* ((ret-tool (run "sh -c 'bin/gf pi --provider kimi-coding -p \"请执行 bash 工具运行 echo hello-kimi\" >/dev/null 2>&1'"
                   :timeout 120
                 ) ;run
       ) ;ret-tool
      ) ;
  (check ret-tool => 0)
) ;let*

(check-report)
