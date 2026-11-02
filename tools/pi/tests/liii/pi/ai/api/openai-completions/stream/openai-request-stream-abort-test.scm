(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (liii go)
  (liii check)
  (liii string)
  (liii time)
  (scheme time)
  (liii subprocess)
  (liii pi abort)
  (liii pi ai message)
  (liii pi ai api openai-completions)
) ;import

(check-set-mode! 'report-failed)

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

;; 读取 stub server 端口：python 启动慢时 PORT 行可能延后，重试读取避免偶发失败
(define (read-stub-port proc)
  (let loop ((n 0) (port #f))
    (cond ((and (not port) (< n 10))
           (loop (+ n 1) (parse-port-line (process-read-line proc)))
          ) ;
          (else port)
    ) ;cond
  ) ;let
) ;define

(define stub-server-file
  (if (file-exists? "tests/stub_gemini_server.py")
    "tests/stub_gemini_server.py"
    (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
      "tools/pi/tests/stub_gemini_server.py"
      "../../tools/pi/tests/stub_gemini_server.py"
    ) ;if
  ) ;if
) ;define

;; 延迟发送 abort 信号的 worker：0.5s 后向通道发中止信号。
;; 顶层 define + (go (fn ch)) 传参形式（闭包变量无法跨 go 会话传输）
(define (delay-abort-signal ch)
  (sleep 0.5)
  (chan-send! ch 'abort 0)
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port (read-stub-port proc))
       (base-url (string-append "http://127.0.0.1:" (number->string port) "/chat-abort-hang"))
      ) ;
  (check-true (number? port))

  ;; 1. 请求发起前已中止：流式 cb 收到首个 chunk 即返回 #f 中止 HTTP，
  ;;    最终返回 stop-reason 为 "aborted" 的 assistant 消息
  (let* ((ch (make-chan 1))
         (cfg (list (cons 'model-id "kimi-mk")
                (cons 'model-meta
                  (list (cons 'base-url base-url) (cons 'max-tokens 1024))
                ) ;list
                (cons 'api-key "mk-kimi-key-1-x")
                (cons 'proxy-url #f)
                (cons 'tools '())
                (cons 'settings '((retry . ((enabled . #f)))))
              ) ;list
             )
         (msgs (list (pi-user-message "ping")))
        ) ;
    (pi-abort-arm-chan! ch)
    (chan-send! ch 'abort 0)
    (let ((resp (openai-request-stream cfg msgs #f '() #f 10)))
      (check (pi-assistant-message? resp) => #t)
      (check (pi-assistant-message-stop-reason resp) => "aborted")
    ) ;let
  ) ;let*

  ;; 2. 流式过程中中止：首个流事件到达时触发中止信号，
  ;;    后续 chunk 被 cb 拒收，最终 stop-reason 为 "aborted"
  (let* ((ch (make-chan 1))
         (cfg (list (cons 'model-id "kimi-mk")
                (cons 'model-meta
                  (list (cons 'base-url base-url) (cons 'max-tokens 1024))
                ) ;list
                (cons 'api-key "mk-kimi-key-1-x")
                (cons 'proxy-url #f)
                (cons 'tools '())
                (cons 'settings '((retry . ((enabled . #f)))))
              ) ;list
             )
         (msgs (list (pi-user-message "ping")))
         (on-event (lambda (ev) (chan-send! ch 'abort 0)))
        ) ;
    (pi-abort-arm-chan! ch)
    (let ((resp (openai-request-stream cfg msgs #f '() on-event 10)))
      (check (pi-assistant-message? resp) => #t)
      (check (pi-assistant-message-stop-reason resp) => "aborted")
    ) ;let
  ) ;let*

  ;; 3. 未中止时流式行为不变：正常返回 stop
  (let* ((ch (make-chan 1))
         (cfg (list (cons 'model-id "kimi-mk")
                (cons 'model-meta
                  (list (cons 'base-url base-url) (cons 'max-tokens 1024))
                ) ;list
                (cons 'api-key "mk-kimi-key-1-x")
                (cons 'proxy-url #f)
                (cons 'tools '())
                (cons 'settings '((retry . ((enabled . #f)))))
              ) ;list
             )
         (msgs (list (pi-user-message "ping")))
        ) ;
    (pi-abort-arm-chan! ch)
    (let ((resp (openai-request-stream cfg msgs #f '() #f 10)))
      (check (pi-assistant-message? resp) => #t)
      (check (pi-assistant-message-stop-reason resp) => "stop")
    ) ;let
  ) ;let*

  ;; 4. 空闲期中止：stub 发 1 个 chunk 后静默挂起 30s，模拟 LLM 思考期。
  ;;    abort 信号晚于 chunk1 到达（0.5s 延迟），之后无新 chunk，
  ;;    无 progress 节拍探测时只能等到 timeout（红灯）；
  ;;    接入 :progress 后约 1s 节拍内中止并结算 aborted（绿灯）
  (let* ((ch (make-chan 1))
         (t0 (current-second))
         (cfg (list (cons 'model-id "chat-abort-hang")
                (cons 'model-meta
                  (list (cons 'base-url base-url) (cons 'max-tokens 1024))
                ) ;list
                (cons 'api-key "mk-kimi-key-1-x")
                (cons 'proxy-url #f)
                (cons 'tools '())
                (cons 'settings '((retry . ((enabled . #f)))))
              ) ;list
             )
         (msgs (list (pi-user-message "ping")))
        ) ;
    (pi-abort-arm-chan! ch)
    (go (delay-abort-signal ch))
    (let ((resp (openai-request-stream cfg msgs #f '() #f 60)))
      (check (pi-assistant-message? resp) => #t)
      (check (pi-assistant-message-stop-reason resp) => "aborted")
    ) ;let
    (check-true (< (- (current-second) t0) 10))
    (pi-abort-reset!)
  ) ;let*

  (pi-abort-reset!)
  (catch #t
    (lambda () (process-kill proc))
    (lambda args #f)
  ) ;catch
) ;let*

(check-report)
