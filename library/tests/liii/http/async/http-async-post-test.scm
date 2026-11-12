;; http-async-post
;; 异步发送 HTTP POST 请求，通过回调函数处理响应。
;;
;; 语法
;; ----
;; (http-async-post url callback :params params :data data
;;                  :headers headers :proxy proxy :timeout timeout)
;;
;; 参数
;; ----
;; url      : string          - 请求的目标 URL
;; callback : procedure       - 回调函数 (lambda (response) ...)
;; params   : alist (可选)    - 查询参数列表
;; data     : string (可选)   - 请求体数据
;; headers  : alist (可选)    - 请求头列表
;; proxy    : alist (可选)    - 代理配置
;; timeout  : real? | integer? | #f (可选) - 总时长超时（秒）：整个请求（含连接阶段）的最长等待时间，
;;            到点必中止；#f 表示不限制
;;
;; 返回值
;; ----
;; 无
;;
;; 描述
;; ----
;; 异步 POST 请求立即返回，不阻塞。请求完成后调用 callback。
;; 超时后请求被中止，回调收到的响应中 error-code 为非零的 curl 错误码。

;; 测试异步 POST 请求

(import (liii check) (liii http async) (liii string) (liii os)
  (liii subprocess)
  (scheme time)
) ;import

(check-set-mode! 'report-failed)

;; 公开接口 http-async-post timeout 参数类型测试
(check-catch 'type-error (http-async-post "http://x/" (lambda (r) #f) :timeout "5"))
(check-catch 'type-error (http-async-post "http://x/" (lambda (r) #f) :timeout 'fast))
(check-catch 'type-error (http-async-post "http://x/" (lambda (r) #f) :timeout #t))
(check-catch 'type-error (http-async-post "http://x/" (lambda (r) #f) :timeout '()))
(check-catch 'value-error (http-async-post "http://x/" (lambda (r) #f) :timeout 0))
(check-catch 'value-error (http-async-post "http://x/" (lambda (r) #f) :timeout -1))

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
  (cond ((file-exists? "library/tests/stub_slow_stream_server.py")
         "library/tests/stub_slow_stream_server.py")
        ((file-exists? "tests/stub_slow_stream_server.py")
         "tests/stub_slow_stream_server.py")
        (else "../../tests/stub_slow_stream_server.py")
  ) ;cond
) ;define

;; timeout 行为测试（本地 stub，无需外部网络）：
;; stub 对 /slow-stream 的 POST 发 1 个 chunk 后挂起 30s，
;; :timeout 2 应使请求提前中止（而非等满 30s），
;; 回调收到 error-code 非零的失败响应；status-code 取决于中止时
;; 响应头是否已到达（可能为 0，也可能为 200），不做断言
(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port (read-stub-port proc))
      ) ;
  (check-true (number? port))

  (let* ((base-url (string-append "http://127.0.0.1:" (number->string port) "/slow-stream"))
         (timeout-completed #f)
         (timeout-response #f)
         (t0 (current-second))
        ) ;
    (http-async-post base-url
      (lambda (response) (set! timeout-completed #t) (set! timeout-response response))
      :timeout
      2
    ) ;http-async-post
    (http-wait-all 30)
    (let ((dt (- (current-second) t0)))
      (check-true timeout-completed)
      (check-true (< dt 10))
      (check-true (> (timeout-response 'error-code) 0))
    ) ;let
  ) ;let*

  (catch #t
    (lambda () (process-kill proc))
    (lambda args #f)
  ) ;catch
) ;let*

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env)
    (exit 0)
  ) ;when
) ;let

(let ((post-completed #f) (post-response #f))
  (http-async-post "https://httpbin.org/post"
    (lambda (response) (set! post-completed #t) (set! post-response response))
    :params
    '()
    :data
    "{\"test\": \"async-post\"}"
    :headers
    '(("Content-Type" . "application/json"))
    :proxy
    '()
  ) ;http-async-post
  (http-wait-all 30)
  (check-true post-completed)
  (check (post-response 'status-code) => 200)
  (check-true (string-contains (post-response 'text) "async-post"))
) ;let

(check-report)
