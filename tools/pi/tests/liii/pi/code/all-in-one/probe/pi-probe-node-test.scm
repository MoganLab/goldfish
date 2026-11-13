;; pi-probe-node
;; 在进程内使用非流式请求向候选节点发送微型 ping 预检。
;;
;; 语法
;; ----
;; (pi-probe-node node [timeout-ms config-dir request-fn opt-retry])
;;
;; 参数
;; ----
;; node : <chain-node>
;; 待探测节点。
;; timeout-ms : number (可选)
;; 请求超时毫秒数。
;; config-dir : (or string path #f) (可选)
;; 隔离配置目录，同时用于按天日志。
;; request-fn : procedure (可选)
;; 测试用请求函数，接收 (provider config msgs sys-prompt tools timeout retry)，
;; 返回 <pi-assistant-message>。
;; opt-retry : alist | #f (可选)
;; 重试配置覆盖；缺省为 #f（遵循 gf pi 已有的重试机制，默认重试 3 次，429 耗尽才判不可用）。
;;
;; 返回值
;; ----
;; boolean
;; 响应正常且存在文本内容时返回 #t；否则记录 FAIL 并返回 #f。

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi ai message)
  (liii pi code all-in-one probe)
  (liii pi code all-in-one alarm)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-probe-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define NODE (make-chain-node "kimi1" "kimi-k3" "high"))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

(define (reset-log!)
  (let ((file (resolve-alarm-log-path TMP-DIR)))
    (when (file-exists? (path->string file))
      (delete-file (path->string file))
    ) ;when
  ) ;let
) ;define

(define (log-text)
  (path-read-text (path->string (resolve-alarm-log-path TMP-DIR)))
) ;define

(define (check-failure status-msg expected-code expected-text)
  (reset-log!)
  (check-false
    (pi-probe-node NODE
      4321
      TMP-DIR
      (lambda (provider config msgs sys-prompt tools timeout retry)
        (pi-assistant-message '()
          :stop-reason "error"
          :error-message status-msg
        ) ;pi-assistant-message
      ) ;lambda
    ) ;pi-probe-node
  ) ;check-false
  (let ((content (log-text)))
    (check-true (string-contains? content " [FAIL] provider=kimi1 model=kimi-k3 "))
    (check-true (string-contains? content (string-append "error=" expected-code " "))
    ) ;check-true
    (check-true (string-contains? content expected-text))
  ) ;let
) ;define

;; 1. 成功请求：非流式收集响应，只携带 ping，不带主会话历史
(let ((captured #f))
  (check-true
    (pi-probe-node NODE
      4321
      TMP-DIR
      (lambda (provider config msgs sys-prompt tools timeout retry)
        (set! captured (list provider config msgs sys-prompt tools timeout retry))
        (pi-assistant-message (list (pi-text-block "pong\n")) "stop")
      ) ;lambda
    ) ;pi-probe-node
  ) ;check-true
  (let* ((provider (list-ref captured 0))
         (config (list-ref captured 1))
         (msgs (list-ref captured 2))
         (sys-prompt (list-ref captured 3))
         (tools (list-ref captured 4))
         (timeout (list-ref captured 5))
         (retry (list-ref captured 6))
        ) ;
    (check provider => "kimi1")
    (check (cdr (assoc 'model-id config)) => "kimi-k3")
    (check (cdr (assoc 'thinking-level config)) => "high")
    (check (length msgs) => 1)
    (check (pi-user-message-content (car msgs)) => "ping")
    (check sys-prompt => #f)
    (check tools => '())
    (check timeout => (/ 4321 1000))
    (check retry => #f)
  ) ;let*
) ;let

;; 2. 429 重试机制：底层按已有机制重试，若在 3 次内自愈成功，探活判定为可用（#t），不记录 FAIL
(let ((simulated-attempts 0))
  (reset-log!)
  (check-true
    (pi-probe-node NODE
      2000
      TMP-DIR
      (lambda (provider config msgs sys-prompt tools timeout retry)
        ;; 模拟真实底层协议的重试机制：只要没禁用重试，前 2 次 429，第 3 次成功
        (let loop ((attempt 1))
          (set! simulated-attempts attempt)
          (if (< attempt 3)
            (loop (+ attempt 1))
            (pi-assistant-message (list (pi-text-block "pong\n")) "stop")
          ) ;if
        ) ;let
      ) ;lambda
    ) ;pi-probe-node
  ) ;check-true
  (check simulated-attempts => 3)
  ;; 成功时不应写入 FAIL 日志
  (check-false (file-exists? (path->string (resolve-alarm-log-path TMP-DIR))))
) ;let

;; 2b. 超时重试机制：底层将超时与 429/0 一同重试，自愈成功则探活可用（#t），不记录 FAIL
(let ((simulated-attempts 0))
  (reset-log!)
  (check-true
    (pi-probe-node NODE
      2000
      TMP-DIR
      (lambda (provider config msgs sys-prompt tools timeout retry)
        (let loop ((attempt 1))
          (set! simulated-attempts attempt)
          (if (< attempt 3)
            (loop (+ attempt 1))
            (pi-assistant-message (list (pi-text-block "pong\n")) "stop")
          ) ;if
        ) ;let
      ) ;lambda
    ) ;pi-probe-node
  ) ;check-true
  (check simulated-attempts => 3)
  (check-false (file-exists? (path->string (resolve-alarm-log-path TMP-DIR))))
) ;let

;; 3. 429 达到 3 次重试上限仍未恢复时，判定为不可用（#f），并记录 429 FAIL 日志
(check-failure "HTTP 429 after 3 attempts: rate limit exceeded" "429" "rate limit")

;; 3b. 超时达到重试上限仍未恢复时，判定为不可用（#f），并记录 timeout FAIL 日志
(check-failure "Request timed out after 8s" "timeout" "Request timed out after 8s")

;; 4. 显式传入 opt-retry 时，能正确透传覆盖（如显式禁用重试）
(let ((captured-retry 'unset))
  (pi-probe-node NODE
    1000
    TMP-DIR
    (lambda (p c m s t to retry)
      (set! captured-retry retry)
      (pi-assistant-message (list (pi-text-block "pong")) "stop")
    ) ;lambda
    '((enabled . #f))
  ) ;pi-probe-node
  (check captured-retry => '((enabled . #f)))
) ;let

;; 5. 常见 HTTP 错误一视同仁，返回 #f 并追加对应错误码
(check-failure "Error: HTTP 529 after 3 attempts: overloaded"
  "529"
  "overloaded"
) ;check-failure
(check-failure "HTTP 429: exceeded_current_quota_error" "429" "quota")
(check-failure "HTTP 401: Invalid API Key" "401" "Invalid API Key")
(check-failure "HTTP 402: insufficient balance" "402" "balance")
(check-failure "Request failed: Could not resolve host" "network" "Could not resolve host")
(check-failure "Request failed: connection timed out" "timeout" "timed out")

;; 3. 超时、网络异常与空响应均不会把失败当作健康
(check-failure "probe timeout waiting for response" "timeout" "timeout")
(reset-log!)
(check-false
  (pi-probe-node NODE
    10
    TMP-DIR
    (lambda args
      (pi-assistant-message '() :stop-reason "aborted" :error-message "timeout")
    ) ;lambda
  ) ;pi-probe-node
) ;check-false
(check-true (string-contains? (log-text) "error=timeout"))
(check-failure "connection refused" "error" "refused")

;; 截断响应（stop=length、正文为空）仍视为链路健康，
;; 因为探活用极小 max_tokens，不应要求必须输出正文
(reset-log!)
(check-true
  (pi-probe-node NODE
    1000
    TMP-DIR
    (lambda args
      (pi-assistant-message '() :stop-reason "length" :error-message #f)
    ) ;lambda
  ) ;pi-probe-node
) ;check-true
(check-false (file-exists? (path->string (resolve-alarm-log-path TMP-DIR))))

;; 4. 请求函数自身抛错也必须收敛为 #f 和一条日志
(reset-log!)
(check-false (pi-probe-node NODE
               1000
               TMP-DIR
               (lambda args (error 'io-error "cannot connect"))
             ) ;pi-probe-node
) ;check-false
(check-true (string-contains? (log-text) "error=exception"))

;; 5. 缺省请求函数在无凭据/隔离目录下安全返回 #f 并记录日志
(reset-log!)
(check-false (pi-probe-node NODE 1000 TMP-DIR))
(check-true (string-contains? (log-text) " [FAIL] provider=kimi1 model=kimi-k3 "))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)