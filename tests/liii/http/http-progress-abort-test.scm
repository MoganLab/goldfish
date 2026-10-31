(import (liii check)
  (liii http)
  (liii go)
  (liii string)
  (liii subprocess)
  (liii os)
  (liii time)
  (scheme time)
) ;import

(check-set-mode! 'report-failed)

;; g_http-post / http-post 的 :progress 回调中止测试 (devel/1227.md)
;;
;; 背景：流式请求只有 WriteCallback（收到 chunk 才触发），LLM 思考期间
;; 长时间无数据到达，用户 ESC 后信号无法被探测，取消明显延迟。
;; :progress 注册 curl ProgressCallback，libcurl 在无数据时也按节拍轮询
;; （poll 超时上限 1000ms），空闲期最长约 1 秒即可探测到中止信号。

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

;; 300ms 后向通道发 abort：模拟用户按 ESC（顶层 define + (go (fn ch)) 传参形式，
;; 零参 lambda 字面量无法跨 go 会话传输 procedure-source）
(define (send-abort-later ch)
  (sleep 0.3)
  (chan-send! ch 'abort 0)
) ;define

(define stub-server-file
  (if (file-exists? "tests/stub_slow_stream_server.py")
    "tests/stub_slow_stream_server.py"
    "../../tests/stub_slow_stream_server.py"
  ) ;if
) ;define

;; :progress 参数类型测试（在真实请求发起前完成，无需网络）
(check-catch 'type-error (http-post "http://x/" :progress 42))
(check-catch 'type-error (http-post "http://x/" :progress #t))
(check-catch 'type-error (http-get "http://x/" :progress 42))

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port (read-stub-port proc))
       (base-url (string-append "http://127.0.0.1:" (number->string port) "/slow-stream"))
      ) ;
  (check-true (number? port))

  ;; 1. g_http-post :progress 返回 #f 中止传输：
  ;;    stub 发 1 个 chunk 后挂起 30s，progress 回调恒返回 #f，
  ;;    请求应在 1~2 秒内返回（而非 30s），error-code 反映回调中止
  (let* ((t0 (current-second))
         (r (g_http-post base-url '() "" '() '() '()
             (lambda (chunk) #t)  ; write-callback：照常收 chunk
             #f #f
             (lambda (dlt dln ult uln) #f)  ; progress：立即中止
           ) ;g_http-post
         ) ;r
         (dt (- (current-second) t0))
        ) ;
    (check-true (< dt 5))
    (check (r 'status-code) => 0)
    (check-true (> (r 'error-code) 0))
    (check-true (string? (r 'reason)))
  ) ;let*

  ;; 2. :progress 返回 #t 不影响正常完成（慢流靠 progress 轮询推进，
  ;;    stub 挂起 30s，此处以 5s timeout 熔断，验证 error 码为超时而非中止）
  (let* ((t0 (current-second))
         (r (g_http-post base-url '() "" '() '() '()
             (lambda (chunk) #t)
             5 #f
             (lambda (dlt dln ult uln) #t)
           ) ;g_http-post
         ) ;r
         (dt (- (current-second) t0))
        ) ;
    (check-true (< dt 8))
    (check-true (> (r 'error-code) 0))
  ) ;let*

  ;; 3. http-post 关键字参数 :progress 透传中止语义：
  ;;    另一 go 协程模拟用户 ESC，300ms 后向通道发 abort，
  ;;    progress 回调探测通道信号后返回 #f，请求应快速返回
  (let* ((ch (make-chan 1))
         (aborted-flag (vector #f))
         (t0 (current-second))
         (r (begin
              (go (send-abort-later ch))
              (http-post base-url
                :stream   #t
                :callback (lambda (chunk) #t)
                :timeout  20
                :progress (lambda (dlt dln ult uln)
                            (if (chan-try-recv! ch #f)
                              (begin (vector-set! aborted-flag 0 #t) #f)
                              #t
                            ) ;if
                          ) ;lambda
              ) ;http-post
            ) ;begin
         ) ;r
         (dt (- (current-second) t0))
        ) ;
    (check-true (< dt 10))
    (check-true (vector-ref aborted-flag 0))
    (check-true (> (r 'error-code) 0))
  ) ;let*

  (catch #t
    (lambda () (process-kill proc))
    (lambda args #f)
  ) ;catch
) ;let*

(check-report)
