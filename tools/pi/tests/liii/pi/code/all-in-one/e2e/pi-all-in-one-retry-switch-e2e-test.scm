;; pi-all-in-one-request-stream 的 429 重试耗尽后切换节点端到端测试
;; 用户 settings.json 配置了 retry 段时，节点 429 重试达到上限后必须切换备用节点，
;; 而不是直接以错误结算整轮任务。
;;
;; 语法
;; ----
;; (pi-all-in-one-request-stream provider config messages system-prompt
;;   tools on-event timeout probe-fn request-fn)
;;
;; 参数
;; ----
;; provider : string，元供应商标识 all-in-one。
;; config : alist，由 resolve-pi-config 解析出的真实会话配置（含 settings）。
;; messages : list，原始会话消息。
;; system-prompt : string，系统提示词。
;; tools、on-event、timeout : any，原样传给真实请求函数。
;; probe-fn、request-fn : procedure，缺省时使用真实协议派发。
;;
;; 返回值
;; ----
;; assistant message
;; 节点 A 429 耗尽后落到节点 B，返回节点 B 的正常响应。
;;
;; 描述
;; ----
;; 1. settings.json 含 retry 段时，首节点持续 429，重试耗尽后切换到备用节点；
;; 2. 未配置 retry 段时行为一致（同样切换备用节点）；
;; 3. 备用节点的探活与真实请求都落到 stub server 的正常模型。

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
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code config settings)
  (liii pi code modes output)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one request)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-aio-retry-switch-e2e-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

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

(define (write-cfg-dir base-url settings-json)
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
  (when settings-json
    (path-write-text (path->string (path-join TMP-DIR "settings.json"))
      settings-json
    ) ;path-write-text
  ) ;when
) ;define

(define (make-config)
  (list (cons 'provider "all-in-one")
    (cons 'provider-api "all-in-one")
    (cons 'model-id "coding-fast")
    (cons 'api-key #f)
    (cons 'config-dir TMP-DIR)
    (cons 'tools '())
    (cons 'settings (pi-load-settings TMP-DIR))
  ) ;list
) ;define

(define LOG-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-aio-retry-switch-logs-" (number->string (getpid)))
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
  ;; HTTP 异常日志重定向到临时目录，便于断言探活请求次数
  (pi-log-init! (path LOG-DIR))

  ;; 1. settings.json 配置 retry 段：429 重试耗尽后切换备用节点
  (write-cfg-dir base-url
    "{\"retry\":{\"enabled\":true,\"maxRetries\":2,\"baseDelayMs\":1,\"provider\":{\"maxRetries\":12}}}"
  ) ;write-cfg-dir
  (pi-chain-reset-sticky-state!)
  (let ((response (pi-all-in-one-request-stream "all-in-one"
                    (make-config)
                    (list (pi-user-message "do the work"))
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
    ;; 探活重试预算与长任务解耦：provider.maxRetries = 12 时探活仍最多重试 3 次
    ;; （1 次首发 + 3 次重试 = 4 次请求）
    (check (count-429-attempts) => 4)
  ) ;let

  ;; 2. 未配置 retry 段：行为一致，同样切换备用节点
  (write-cfg-dir base-url #f)
  (pi-chain-reset-sticky-state!)
  (let ((response (pi-all-in-one-request-stream "all-in-one"
                    (make-config)
                    (list (pi-user-message "do the work"))
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
  ) ;let

  (process-kill proc)
) ;let*

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(when (file-exists? LOG-DIR)
  (rmtree LOG-DIR #t)
) ;when

(check-report)
