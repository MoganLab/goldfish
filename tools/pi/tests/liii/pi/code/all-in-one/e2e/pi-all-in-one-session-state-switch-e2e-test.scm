;; all-in-one 会话在第 2 条消息起仍走链式调度的端到端测试
;; 会话里上一轮 assistant 消息记录的是实际服务节点（物理节点），
;; 该记录不得覆盖 all-in-one 伪模型：一旦被覆盖，请求绕开链式调度，
;; 节点 429 重试耗尽后不再切换备用节点。
;;
;; 语法
;; ----
;; (pi-build-session-projection entries)
;;   -> (values messages state diagnostics)
;; (pi-resolve-session-state cfg state parsed)
;;   -> (values cfg warnings)
;; (pi-all-in-one-request-stream provider config messages system-prompt
;;   tools on-event timeout probe-fn request-fn)
;;
;; 参数
;; ----
;; 无（本文件为端到端测试，参数由内部 fixture 与本地 stub server 提供）。
;;
;; 返回值
;; ----
;; 1. 投影状态里的模型来源是 assistant（实际服务节点）；
;; 2. 合并后 cfg 的 provider 仍是 all-in-one、model-id 仍是伪模型 id；
;; 3. 首节点 429 重试耗尽后切换到备用节点，返回备用节点的正常响应。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii subprocess)
  (liii pi ai message)
  (liii pi ai log)
  (liii pi code config)
  (liii pi code config settings)
  (liii pi code core compaction session)
  (liii pi code core session-projection)
  (liii pi code modes output)
  (liii pi code session-state)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one request)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-aio-session-state-e2e-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define SESSION-FILE (path->string (path-join TMP-DIR "session.jsonl")))

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
      "../tools/pi/tests/stub_gemini_server.py"
    ) ;if
  ) ;if
) ;define

(define (write-cfg-dir base-url)
  (when (file-exists? TMP-DIR)
    (rmtree TMP-DIR #t)
  ) ;when
  (path-mkdir (path (path-join TMP-DIR "models" "google"))
    :parents  #t
    :exist-ok #t
  ) ;path-mkdir
  (path-write-text (path->string (path-join TMP-DIR "models" "providers.json"))
    "[\"google\"]"
  ) ;path-write-text
  (path-write-text (path->string (path-join TMP-DIR "auth.json"))
    "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-key\"}}"
  ) ;path-write-text
  (path-write-text (path->string (path-join TMP-DIR "models" "google.json"))
    (string-append "{\"api\":\"google-generative-ai\",\"baseUrl\":\""
      base-url
      "\",\"models\":[\"429-always\",\"ok-model\"]}"
    ) ;string-append
  ) ;path-write-text
  (path-write-text (path->string (path-join TMP-DIR "models" "google" "429-always.json"))
    "{\"id\":\"429-always\",\"name\":\"429-always\"}"
  ) ;path-write-text
  (path-write-text (path->string (path-join TMP-DIR "models" "google" "ok-model.json"))
    "{\"id\":\"ok-model\",\"name\":\"ok-model\"}"
  ) ;path-write-text
  ;; 首节点持续 429，备用节点正常返回
  (path-write-text (path->string (path-join TMP-DIR "all-in-one.json"))
    "{\"provider\":\"all-in-one\",\"continuous\":false,\"probeTimeout\":5,\"retryInterval\":60,\"chains\":[{\"id\":\"coding-fast\",\"nodes\":[{\"provider\":\"google\",\"model\":\"429-always\"},{\"provider\":\"google\",\"model\":\"ok-model\"}]}]}"
  ) ;path-write-text
  ;; provider.maxRetries = 2：真实请求共尝试 3 次后判定 429 耗尽
  (path-write-text (path->string (path-join TMP-DIR "settings.json"))
    "{\"retry\":{\"enabled\":true,\"maxRetries\":2,\"baseDelayMs\":1,\"provider\":{\"maxRetries\":2}}}"
  ) ;path-write-text
  ;; 会话历史：上一轮请求落在物理节点 ok-model 上，assistant 消息记录该节点
  (path-write-text SESSION-FILE
    (string-append "{\"type\":\"session\",\"version\":3,\"id\":\"sess-aio\",\"timestamp\":\"2026-11-18T00:00:00.000Z\",\"cwd\":\"/tmp/aio\"}\n"
      "{\"type\":\"message\",\"id\":\"u1\",\"parentId\":null,\"timestamp\":\"2026-11-18T00:00:01.000Z\",\"message\":{\"role\":\"user\",\"content\":\"first\"}}\n"
      "{\"type\":\"message\",\"id\":\"a1\",\"parentId\":\"u1\",\"timestamp\":\"2026-11-18T00:00:02.000Z\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"ok\"}],\"stopReason\":\"stop\",\"provider\":\"google\",\"model\":\"ok-model\"}}\n"
    ) ;string-append
  ) ;path-write-text
) ;define

(define (make-cfg)
  (make-pi-config
    :provider     "all-in-one"
    :provider-api "all-in-one"
    :model-id     "coding-fast"
    :api-key      #f
    :config-dir   (path TMP-DIR)
    :tools        '()
    :settings     (pi-load-settings TMP-DIR)
  ) ;make-pi-config
) ;define

(define LOG-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-aio-session-state-logs-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define (count-429-attempts)
  (let* ((log-file (path->string (pi-log-file-path)))
         (text (if (file-exists? log-file) (path-read-text log-file) ""))
         (lines
           (filter (lambda (line)
                     (and (string-contains? line "model=429-always")
                       (string-contains? line "status=429")
                     ) ;and
                   ) ;lambda
             (string-split text "\n")
           ) ;filter
         ) ;lines
        ) ;
    (length lines)
  ) ;let*
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port (parse-port-line (process-read-line proc)))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (check-true (number? port))
  ;; HTTP 异常日志重定向到临时目录，便于断言 429 尝试次数
  (pi-log-init! (path LOG-DIR))
  (write-cfg-dir base-url)
  (pi-chain-reset-sticky-state!)

  ;; 1. 投影状态：模型来源是 assistant 消息（实际服务节点）
  (let-values (((messages state diagnostics)
                (pi-build-session-projection (pi-read-session-entries SESSION-FILE))
               ) ;
              ) ;
    (check-true (pair? messages))
    (check-true (pair? state))
    (check (cdr (assoc 'modelSource state)) => "assistant")
    (check
      (cdr (assoc 'provider (cdr (assoc 'model state))))
      =>
      "google"
    ) ;check
  ) ;let-values

  ;; 2. 合并会话状态后，cfg 仍是 all-in-one 伪模型（不被物理节点覆盖）
  (let-values (((_messages state _diagnostics)
                (pi-build-session-projection (pi-read-session-entries SESSION-FILE))
               ) ;
              ) ;
    (let-values (((cfg warnings) (pi-resolve-session-state (make-cfg) state '())))
      (check (pi-cfg-ref cfg 'provider) => "all-in-one")
      (check (pi-cfg-ref cfg 'model-id) => "coding-fast")
      (check warnings => '())
      ;; 3. 走 all-in-one 链式调度：首节点 429 重试耗尽后切换备用节点
      (let ((response (pi-all-in-one-request-stream "all-in-one"
                        cfg
                        (list (pi-user-message "second"))
                        "system"
                        '()
                        #f
                        30
                        #f
                        #f
                      ) ;pi-all-in-one-request-stream
            ) ;response
           ) ;
        (check (pi-assistant-message-stop-reason response) => "stop")
        (check (pi-content-text (pi-assistant-message-content response))
          =>
          "Hello stream!"
        ) ;check
        ;; provider.maxRetries = 2：1 次首发 + 2 次重试 = 3 次 429 后切换
        (check (count-429-attempts) => 3)
      ) ;let
    ) ;let-values
  ) ;let-values

  (process-kill proc)
) ;let*

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(when (file-exists? LOG-DIR)
  (rmtree LOG-DIR #t)
) ;when

(check-report)
