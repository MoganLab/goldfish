(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (liii go)
  (liii check)
  (liii string)
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

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port (read-stub-port proc))
       (base-url (string-append "http://127.0.0.1:" (number->string port) "/mk-kimi-plain"))
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

  (pi-abort-reset!)
  (catch #t
    (lambda () (process-kill proc))
    (lambda args #f)
  ) ;catch
) ;let*

(check-report)
