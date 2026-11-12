;; 等待期间用户直接输入消息的中止行为端到端测试
;; 全链枯竭进入等待后，用户在输入框发消息应立即中止等待并让前台接手该消息，
;; 而不是继续重试/继续等待到 retryIntervalMs 或 maxWaitMs。
;;
;; 语法
;; ----
;; (pi-all-in-one-request-stream provider config messages system-prompt
;;   tools on-event timeout probe-fn request-fn)
;;
;; 参数
;; ----
;; provider : string，元供应商标识 all-in-one。
;; config : alist，含 config-dir、model-id、tools 与注入的
;;   now-fn / sleep-fn / aborted? / get-steering-messages。
;; messages : list，原始会话消息。
;; system-prompt : string，系统提示词。
;; tools、on-event、timeout : any，原样传给真实请求函数。
;; probe-fn : procedure，stub 探活（始终失败，强制进入等待）。
;; request-fn : procedure，stub 业务请求。
;;
;; 返回值
;; ----
;; assistant message
;; 用户消息到达时返回 stop reason 为 aborted 的消息。
;;
;; 描述
;; ----
;; 1. 等待首个步进就检测到用户消息，时钟不推进（不等 retryIntervalMs）；
;; 2. 不触发任何到期重试，也不调用业务请求；
;; 3. 用户消息只被消费一次；
;; 4. 已有 active_node 时切换思考等级，立即显示所选等级。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii tui)
  (liii tui component)
  (liii tui text-width)
  (liii pi ai message)
  (liii pi code modes interactive)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one request)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-aio-wait-abort-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path (path-join TMP-DIR "models" "stub")) :parents #t :exist-ok #t)
(path-write-text (path->string (path-join TMP-DIR "all-in-one.json"))
  "{\"provider\":\"all-in-one\",\"continuous\":true,\"retryIntervalMs\":600000,\"maxWaitMs\":600000,\"probeTimeoutMs\":1000,\"chains\":[{\"id\":\"coding-fast\",\"nodes\":[{\"provider\":\"stub\",\"model\":\"one\"}]}]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "providers.json")) "[\"stub\"]")
(path-write-text (path->string (path-join TMP-DIR "auth.json"))
  "{\"stub\":{\"type\":\"api_key\",\"key\":\"k\"}}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub.json"))
  "{\"api\":\"openai-completions\",\"baseUrl\":\"http://127.0.0.1:1\",\"models\":[\"one\"]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub" "one.json"))
  "{\"id\":\"one\",\"name\":\"one\",\"reasoning\":true}"
) ;path-write-text

(define (make-config . extra)
  (append extra
    (list (cons 'provider "all-in-one")
      (cons 'provider-api "all-in-one")
      (cons 'model-id "coding-fast")
      (cons 'api-key #f)
      (cons 'config-dir TMP-DIR)
      (cons 'tools '())
    ) ;list
  ) ;append
) ;define

;; 虚拟时钟：sleep 只推进 now，不真实阻塞
(define (make-clock)
  (let ((now 1000000))
    (cons (lambda () now) (lambda (ms) (set! now (+ now ms))))
  ) ;let
) ;define

;; ---------------------------------------------------------------------------
;; 1. 用户消息在首个步进即到达：立即返回 aborted，不推进时钟、不重试
;; ---------------------------------------------------------------------------
(pi-chain-reset-sticky-state!)
(let* ((clock (make-clock))
       (pending (list "typed immediately"))
       (consumed '())
       (request-called #f)
       (config (make-config
                 (cons 'now-fn (car clock))
                 (cons 'sleep-fn (cdr clock))
                 (cons 'aborted? (lambda () #f))
                 (cons 'get-steering-messages
                   (lambda ()
                     (if (pair? pending)
                       (let ((m (car pending)))
                         (set! pending (cdr pending))
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
            (list (pi-user-message "start"))
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
    ;; 没有任何实际等待（时钟未推进，说明未 sleep 到 retryIntervalMs）
    (check (- ((car clock)) 1000000) => 0)
    ;; 未触发业务请求，用户消息只消费一次
    (check-false request-called)
    (check consumed => '("typed immediately"))
  ) ;let
) ;let*

;; ---------------------------------------------------------------------------
;; 2. 用户消息在等待若干步进后到达：立即结束，不等满 maxWaitMs
;; ---------------------------------------------------------------------------
(pi-chain-reset-sticky-state!)
(let* ((clock (make-clock))
       (pending (list "typed later"))
       (poll-count 0)
       (config (make-config
                 (cons 'now-fn (car clock))
                 (cons 'sleep-fn (cdr clock))
                 (cons 'aborted? (lambda () #f))
                 (cons 'step-ms 100)
                 (cons 'get-steering-messages
                   (lambda ()
                     ;; 前 5 次轮询为空，模拟用户过一会儿才输入（远早于到期重试）
                     (set! poll-count (+ poll-count 1))
                     (if (and (> poll-count 5) (pair? pending))
                       (let ((m (car pending)))
                         (set! pending (cdr pending))
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
            (list (pi-user-message "start"))
            ""
            '()
            #f
            60
            (lambda (node timeout-ms dir request-fn) #f)
            (lambda args (pi-assistant-message '() "stop"))
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "aborted")
    ;; 远小于 maxWaitMs(600000ms) 就结束
    (check (< (- ((car clock)) 1000000) 1000) => #t)
  ) ;let
) ;let*

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
