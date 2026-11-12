;; pi-all-in-one-request-stream 的全链恢复端到端测试
;; 用进程内 stub 供应商与 stub 探活验证全链枯竭后的等待、续跑与等级收敛。
;;
;; 语法
;; ----
;; (pi-all-in-one-request-stream provider config messages system-prompt
;;   tools on-event timeout probe-fn request-fn)
;;
;; 参数
;; ----
;; provider : string，元供应商标识 all-in-one。
;; config : alist，含 config-dir、model-id、tools 以及注入的
;;   now-fn / sleep-fn / aborted? / emit / get-steering-messages。
;; messages : list，原始会话消息。
;; system-prompt : string，系统提示词。
;; tools、on-event、timeout : any，原样传给真实请求函数。
;; probe-fn : procedure，stub 探活；前两轮全部失败，第三轮起成功。
;; request-fn : procedure，stub 业务请求，记录收到的节点配置与消息。
;;
;; 返回值
;; ----
;; assistant message
;; 首轮全链枯竭时进入等待，节点恢复后返回真实响应。
;;
;; 描述
;; ----
;; 1. 恢复后只发一次业务请求，上下文仍是原始用户消息，无伪造 Auto-Continue；
;; 2. continuous = #f 时立即失败且不调用 sleep；
;; 3. 等待中 Esc 中止返回 aborted 且不再发业务请求；
;; 4. 等待中 Enter steering 只消费一次并随原上下文进入请求；
;; 5. 显式 --thinking high 经异构节点故障转移时逐节点收敛；
;; 6. 无 --thinking 的对照组使用各节点默认等级；
;; 7. 日志含 [EXHAUSTED] 且不出现 Auto-Continue。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi ai message)
  (liii pi code modes output)
  (liii pi code all-in-one alarm)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one request)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-aio-recovery-e2e-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path (path-join TMP-DIR "models" "stub")) :parents #t :exist-ok #t)

;; 进程内 stub 供应商：不依赖真实 API Key，不访问公网。
;; continuous 打开，重试间隔 500ms，单次等待预算 5000ms。
(path-write-text (path->string (path-join TMP-DIR "all-in-one.json"))
  "{\"provider\":\"all-in-one\",\"continuous\":true,\"retryIntervalMs\":500,\"maxWaitMs\":5000,\"probeTimeoutMs\":1000,\"chains\":[{\"id\":\"coding-fast\",\"nodes\":[{\"provider\":\"stub\",\"model\":\"one\",\"thinkingLevel\":\"low\"},{\"provider\":\"stub\",\"model\":\"two\",\"thinkingLevel\":\"high\"}]}]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "providers.json"))
  "[\"stub\"]"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "auth.json"))
  "{\"stub\":{\"type\":\"api_key\",\"key\":\"fixture-key\"}}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub.json"))
  "{\"api\":\"openai-completions\",\"baseUrl\":\"http://127.0.0.1:1\",\"models\":[\"one\",\"two\"]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub" "one.json"))
  "{\"id\":\"one\",\"name\":\"one\",\"reasoning\":true}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub" "two.json"))
  "{\"id\":\"two\",\"name\":\"two\",\"reasoning\":true,\"thinkingLevelMap\":{\"medium\":null}}"
) ;path-write-text

(define (make-config . opt-extra)
  (append opt-extra
    (list (cons 'provider "all-in-one")
      (cons 'provider-api "all-in-one")
      (cons 'model-id "coding-fast")
      (cons 'api-key #f)
      (cons 'config-dir TMP-DIR)
      (cons 'tools '())
    ) ;list
  ) ;append
) ;define

;; 虚拟时钟：所有等待立即推进 now，sleep 只推进时钟不真实阻塞

(define (make-clock)
  (let ((now 1000000))
    (cons (lambda () now) (lambda (ms) (set! now (+ now ms))))
  ) ;let
) ;define

;; ---------------------------------------------------------------------------
;; 1. 全链首次枯竭 → 进入 recovery 等待 → 节点恢复后用原上下文续跑
;; ---------------------------------------------------------------------------
(pi-chain-reset-sticky-state!)
(let* ((clock (make-clock))
       (now-fn (car clock))
       (sleep-fn (cdr clock))
       (probe-round 0)
       (attempts 0)
       (captured-msgs '())
       (events '())
       (config (make-config (cons 'now-fn now-fn)
                 (cons 'sleep-fn sleep-fn)
                 (cons 'aborted? (lambda () #f))
               ) ;make-config
       ) ;config
      ) ;
  ;; 第一轮探活全部失败；recovery 唤醒后的第二轮探活全部成功
  (let ((response
          (pi-all-in-one-request-stream "all-in-one"
            (cons
              (cons 'emit
                (lambda (type . payload) (set! events (cons (cons type payload) events)))
              ) ;cons
              config
            ) ;cons
            (list (pi-user-message "do the work"))
            "system"
            '()
            #f
            60
            (lambda (node timeout-ms dir request-fn)
              (set! probe-round (+ probe-round 1))
              ;; 首轮：所有节点探活失败
              (>= probe-round 3)
            ) ;lambda
            (lambda (provider node-cfg messages system-prompt tools on-event timeout)
              (set! attempts (+ attempts 1))
              (set! captured-msgs messages)
              (pi-assistant-message (list (pi-text-block "recovered ok")) "stop")
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    ;; recovery 恢复后返回真实响应而非枯竭错误
    (check (pi-assistant-message-stop-reason response) => "stop")
    (check (pi-content-text (pi-assistant-message-content response))
      =>
      "recovered ok"
    ) ;check
    ;; 恢复后只发一次业务请求，且上下文仍是原始用户消息（不含伪造的 Auto-Continue）
    (check attempts => 1)
    (check (length captured-msgs) => 1)
    (check (pi-message-field (car captured-msgs) 'role "?") => "user")
    (check (pi-message-field (car captured-msgs) 'content "?") => "do the work")
    ;; 等待事件序列包含 wait/tick/retry/end
    (let ((types (map car events)))
      (check-true (if (memq 'recovery_wait types) #t #f))
      (check-true (if (memq 'recovery_tick types) #t #f))
      (check-true (if (memq 'recovery_retry types) #t #f))
      (check-true (if (memq 'recovery_end types) #t #f))
    ) ;let
  ) ;let
) ;let*

;; ---------------------------------------------------------------------------
;; 2. continuous = #f 时保持快速失败，不进入等待
;; ---------------------------------------------------------------------------
(pi-chain-reset-sticky-state!)
(let* ((clock (make-clock))
       (sleep-called #f)
       (config
         (make-config (cons 'now-fn (car clock))
           (cons 'sleep-fn (lambda (ms) (set! sleep-called #t)))
         ) ;make-config
       ) ;config
       (content (path-read-text (path->string (path-join TMP-DIR "all-in-one.json"))))
      ) ;
  ;; 临时把配置改成 continuous=false
  (path-write-text (path->string (path-join TMP-DIR "all-in-one.json"))
    (string-append (substring content 0 (string-position "\"continuous\":true" content))
      "\"continuous\":false"
      (substring content
        (+ (string-position "\"continuous\":true" content)
          (string-length "\"continuous\":true")
        ) ;+
        (string-length content)
      ) ;substring
    ) ;string-append
  ) ;path-write-text
  (let ((response (pi-all-in-one-request-stream "all-in-one"
                    config
                    (list (pi-user-message "fail fast"))
                    "system"
                    '()
                    #f
                    60
                    (lambda (node timeout-ms dir request-fn) #f)
                    (lambda args (pi-assistant-message '() "stop"))
                  ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "error")
    (check-false sleep-called)
  ) ;let
  ;; 还原配置
  (path-write-text (path->string (path-join TMP-DIR "all-in-one.json")) content)
) ;let*

;; ---------------------------------------------------------------------------
;; 3. 等待期间 Esc 中止：返回 aborted，不再发业务请求
;; ---------------------------------------------------------------------------
(pi-chain-reset-sticky-state!)
(let* ((clock (make-clock))
       (request-called #f)
       (config (make-config (cons 'now-fn (car clock))
                 (cons 'sleep-fn (cdr clock))
                 (cons 'aborted? (lambda () #t))
               ) ;make-config
       ) ;config
      ) ;
  (let ((response (pi-all-in-one-request-stream "all-in-one"
                    config
                    (list (pi-user-message "abort me"))
                    "system"
                    '()
                    #f
                    60
                    (lambda (node timeout-ms dir request-fn) #f)
                    (lambda args (set! request-called #t) (pi-assistant-message '() "stop"))
                  ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "aborted")
    (check-false request-called)
  ) ;let
) ;let*

;; ---------------------------------------------------------------------------
;; 4. 等待期间 Enter steering：立即唤醒，只注入一次并携带在请求上下文中
;; ---------------------------------------------------------------------------
(pi-chain-reset-sticky-state!)
(let* ((clock (make-clock))
       (steering-queue (list "steer once"))
       (consumed '())
       (probe-round 0)
       (captured-msgs '())
       (config
         (make-config (cons 'now-fn (car clock))
           (cons 'sleep-fn (cdr clock))
           (cons 'aborted? (lambda () #f))
           (cons 'get-steering-messages
             (lambda ()
               (if (pair? steering-queue)
                 (let ((m (car steering-queue)))
                   (set! steering-queue (cdr steering-queue))
                   (set! consumed (cons m consumed))
                   (list m)
                 ) ;let
                 '()
               ) ;if
             ) ;lambda
           ) ;cons
         ) ;make-config
       ) ;config
      ) ;
  (let ((response
          (pi-all-in-one-request-stream "all-in-one"
            config
            (list (pi-user-message "first"))
            "system"
            '()
            #f
            60
            (lambda (node timeout-ms dir request-fn)
              (set! probe-round (+ probe-round 1))
              (> probe-round 2)
            ) ;lambda
            (lambda (provider node-cfg messages system-prompt tools on-event timeout)
              (set! captured-msgs messages)
              (pi-assistant-message (list (pi-text-block "steered ok")) "stop")
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "stop")
    (check consumed => '("steer once"))
    ;; 原上下文 + 一条 steering 用户消息，且 steering 只出现一次
    (check (length captured-msgs) => 2)
    (check (pi-message-field (car captured-msgs) 'content "?") => "first")
    (check (pi-message-field (cadr captured-msgs) 'content "?") => "steer once")
  ) ;let
) ;let*

;; ---------------------------------------------------------------------------
;; 5. 显式 --thinking high 经异构节点故障转移：每个节点收到各自合法等级
;; ---------------------------------------------------------------------------
(pi-chain-reset-sticky-state!)
(let ((seen '()))
  (let ((response
          (pi-all-in-one-request-stream "all-in-one"
            (make-config (cons 'thinking-level "high") (cons 'thinking-level-explicit? #t))
            (list (pi-user-message "think hard"))
            "system"
            '()
            #f
            60
            (lambda (node timeout-ms dir request-fn) #t)
            (lambda (provider node-cfg messages system-prompt tools on-event timeout)
              (let ((model-id (cdr (assoc 'model-id node-cfg))))
                (set! seen (cons (cons model-id (cdr (assoc 'thinking-level node-cfg))) seen))
                (if (equal? model-id "one")
                  (pi-assistant-message '() "error" "HTTP 503: unavailable")
                  (pi-assistant-message (list (pi-text-block "ok")) "stop")
                ) ;if
              ) ;let
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "stop")
    ;; 节点 one 支持 high；节点 two 最高只支持 high（medium 为 null），显式 high 原样保留
    (check (reverse seen) => '(("one" . "high") ("two" . "high")))
  ) ;let
) ;let

;; ---------------------------------------------------------------------------
;; 6. 无显式 --thinking 的对照组：节点各自使用 all-in-one.json 的默认等级
;; ---------------------------------------------------------------------------
(pi-chain-reset-sticky-state!)
(let ((seen '()))
  (let ((response
          (pi-all-in-one-request-stream "all-in-one"
            (make-config)
            (list (pi-user-message "default level"))
            "system"
            '()
            #f
            60
            (lambda (node timeout-ms dir request-fn) #t)
            (lambda (provider node-cfg messages system-prompt tools on-event timeout)
              (let ((model-id (cdr (assoc 'model-id node-cfg))))
                (set! seen (cons (cons model-id (cdr (assoc 'thinking-level node-cfg))) seen))
                (if (equal? model-id "one")
                  (pi-assistant-message '() "error" "HTTP 503: unavailable")
                  (pi-assistant-message (list (pi-text-block "ok")) "stop")
                ) ;if
              ) ;let
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "stop")
    ;; 节点 one 默认 low；节点 two 默认 high
    (check (reverse seen) => '(("one" . "low") ("two" . "high")))
  ) ;let
) ;let

;; ---------------------------------------------------------------------------
;; 7. 日志包含 EXHAUSTED 与恢复过程（无 Auto-Continue 痕迹）
;; ---------------------------------------------------------------------------
(let ((log-file (resolve-alarm-log-path TMP-DIR)))
  (check-true (file-exists? log-file))
  (let ((content (path-read-text log-file)))
    (check-true (string-contains? content "[EXHAUSTED]"))
    (check-true (string-contains? content "coding-fast"))
    (check-false (string-contains? content "Auto-Continue"))
  ) ;let
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
