(import (liii check)
  (liii http)
  (liii string)
  (liii os)
  (scheme file)
  (scheme time)
) ;import

(check-set-mode! 'report-failed)

;; 公开接口 http-get 参数类型测试 (devel/0149.md)
(check-catch 'type-error (http-get 42))
(check-catch 'type-error (http-get #\a))
(check-catch 'type-error (http-get 'invalid))
(check-catch 'type-error (http-get "http://x/" :params 42))
(check-catch 'type-error (http-get "http://x/" :params '(42)))
(check-catch 'type-error (http-get "http://x/" :headers 42))
(check-catch 'type-error (http-get "http://x/" :headers '(42)))
(check-catch 'type-error (http-get "http://x/" :proxy 42))
(check-catch 'type-error (http-get "http://x/" :proxy '(42)))
(check-catch 'type-error (http-get "http://x/" :timeout "5"))
(check-catch 'type-error (http-get "http://x/" :timeout 'fast))
(check-catch 'type-error (http-get "http://x/" :timeout #t))
(check-catch 'type-error (http-get "http://x/" :timeout '()))
(check-catch 'value-error (http-get "http://x/" :timeout 0))
(check-catch 'value-error (http-get "http://x/" :timeout 0.0))
(check-catch 'value-error (http-get "http://x/" :timeout -1))
(check-catch 'value-error (http-get "http://x/" :timeout -0.5))
(check-catch 'type-error (http-get "http://x/" :idle-timeout "5"))
(check-catch 'type-error (http-get "http://x/" :idle-timeout 'fast))
(check-catch 'type-error (http-get "http://x/" :idle-timeout #t))
(check-catch 'type-error (http-get "http://x/" :idle-timeout '()))
(check-catch 'value-error (http-get "http://x/" :idle-timeout 0))
(check-catch 'value-error (http-get "http://x/" :idle-timeout 0.0))
(check-catch 'value-error (http-get "http://x/" :idle-timeout -1))
(check-catch 'value-error (http-get "http://x/" :idle-timeout -0.5))

;; C 层入口 g_http-get 的 timeout 与 idle-timeout 参数类型测试
(check-catch 'type-error (g_http-get "http://x/" '() '() '() #f "5"))
(check-catch 'type-error (g_http-get "http://x/" '() '() '() #f 'fast))
(check-catch 'value-error (g_http-get "http://x/" '() '() '() #f 0))
(check-catch 'value-error (g_http-get "http://x/" '() '() '() #f -1))
(check-catch 'type-error (g_http-get "http://x/" '() '() '() #f #f "5"))
(check-catch 'type-error (g_http-get "http://x/" '() '() '() #f #f 'fast))
(check-catch 'value-error (g_http-get "http://x/" '() '() '() #f #f 0))
(check-catch 'value-error (g_http-get "http://x/" '() '() '() #f #f -1))

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env)
    (check-report)
    (exit 0)
  ) ;when
) ;let

;; 不可达网络地址短超时熔断
(let* ((t0 (current-second))
       (r (http-get "http://10.255.255.1" :timeout 0.5 :idle-timeout 1))
       (dt (- (current-second) t0))
      ) ;
  (check (r 'status-code) => 0)
  (check-true (< dt 2.5))
  (check-true (integer? (r 'error-code)))
  (check-true (> (r 'error-code) 0))
  (check-true (string? (r 'reason)))
  (check-true (> (string-length (r 'reason)) 0))
) ;let*

(define (binary-file-size path)
  (let ((port (open-binary-input-file path)))
    (dynamic-wind (lambda () #f)
      (lambda ()
        (let loop
          ((chunk (read-string 8192 port)) (n 0))
          (if (eof-object? chunk)
            n
            (loop (read-string 8192 port) (+ n (string-length chunk)))
          ) ;if
        ) ;let
      ) ;lambda
      (lambda () (close-port port))
    ) ;dynamic-wind
  ) ;let
) ;define



;; http-get
;; 发送 HTTP GET 请求，返回响应对象。
;;
;; 语法
;; ----
;; (http-get url :params params :headers headers :proxy proxy
;;           :output-file output-file :stream stream :callback callback
;;           :timeout timeout :idle-timeout idle-timeout)
;;
;; 参数
;; ----
;; url         : string          - 请求的目标 URL
;; params      : alist (可选)    - 查询参数列表，如 '(("key" . "value"))
;; headers     : alist (可选)    - 请求头列表
;; proxy       : alist (可选)    - 代理配置
;; output-file : string (可选)   - stream 模式下将响应体按 chunk 写入本地文件
;; stream      : boolean (可选)  - 启用简单流式模式
;; callback    : procedure (可选)- 按 chunk 处理响应体 (lambda (chunk) ...)
;; timeout     : real? | integer? | #f (可选) - 总时长超时（秒）：整个请求的最长等待时间，到点必中止
;; idle-timeout: real? | integer? | #f (可选) - 空闲超时（秒）：连续 N 秒无有效数据传输则中止，不限制总时长
;;
;; 返回值
;; ----
;; response-object（哈希表，包含 status-code, url, elapsed, text, reason, headers, error-code 等字段）。
;;
;; 描述
;; ----
;; GET 请求用于从服务器获取资源。查询参数会自动编码并附加到 URL。
;; 当 :stream 为 #t 时，会在 Scheme 层按 chunk 处理响应体，并在请求完成后返回 response-object。
;;
;; timeout 与 idle-timeout 的区别：
;; 1. :timeout 是总时长超时，限制整个请求（含连接阶段）的最长等待时间，到点必中止
;; 2. :idle-timeout 是空闲超时，仅在传输停死（连续 N 秒传输速度低于 1 B/s）时中止，不限制总时长
;; 3. 两者可搭配使用：timeout 兜底总时长，idle-timeout 快速发现挂死且不惩罚慢而健康的传输

;; 基本 GET 请求
(let ((r (http-get "https://httpbin.org")))
  (check (r 'status-code) => 200)
  (check-true (> (string-length (r 'text)) 0))
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8")
) ;let

;; 带查询参数的 GET 请求
(let ((r (http-get "https://httpbin.org/get"
           :params '(("key1" . "value1") ("key2" . "value2"))
         ) ;http-get
      ) ;r
     ) ;
  (check-true (string-contains (r 'text) "value1"))
  (check-true (string-contains (r 'text) "value2"))
  (check (r 'url) => "https://httpbin.org/get?key1=value1&key2=value2")
) ;let

;; 通过 :stream #t + :callback 流式处理响应体
(let ((collected '()))
  (let ((r
          (http-get "https://httpbin.org/get"
            :stream #t
            :params '(("download" . "stream"))
            :callback
            (lambda (chunk)
              (when (> (string-length chunk) 0)
                (set! collected (cons chunk collected))
              ) ;when
              #t
            ) ;lambda
          ) ;http-get
        ) ;r
       ) ;
    (check-true (hash-table? r))
    (check (r 'status-code) => 200)
    (check-true (> (length collected) 0))
    (let ((body (string-join (reverse collected) "")))
      (check-true (string-contains body "stream"))
    ) ;let
  ) ;let
) ;let

;; 通过 :stream #t + :headers 流式处理响应体
(let ((collected '()))
  (let ((r
          (http-get "https://httpbin.org/headers"
            :stream  #t
            :headers '(("X-Goldfish-Test" . "stream-get"))
            :callback
            (lambda (chunk)
              (when (> (string-length chunk) 0)
                (set! collected (cons chunk collected))
              ) ;when
              #t
            ) ;lambda
          ) ;http-get
        ) ;r
       ) ;
    (check-true (hash-table? r))
    (check (r 'status-code) => 200)
    (check-true (> (length collected) 0))
    (let ((body (string-join (reverse collected) "")))
      (check-true (string-contains body "X-Goldfish-Test"))
      (check-true (string-contains body "stream-get"))
    ) ;let
  ) ;let
) ;let

;; 通过 :stream #t + :output-file 将响应体按 chunk 写入本地文件
(let* ((output-file "/tmp/goldfish-http-get-download.txt")
       (r (begin
            (when (file-exists? output-file)
              (delete-file output-file)
            ) ;when
            (http-get "https://jsonplaceholder.typicode.com/posts/1"
              :stream      #t
              :output-file output-file
            ) ;http-get
          ) ;begin
       ) ;r
      ) ;
  (check-true (hash-table? r))
  (check (r 'status-code) => 200)
  (check-true (file-exists? output-file))
  (let ((body (call-with-input-file output-file (lambda (port) (read-string 4096 port)))
        ) ;body
       ) ;
    (check-true (string-contains body "\"userId\""))
  ) ;let
  (delete-file output-file)
) ;let*

;; 通过 :stream #t + :callback 下载较大的流式响应（100KB），验证 chunk 累计长度
(let ((total-bytes 0) (chunks 0))
  (let ((r
          (http-get "https://httpbin.org/stream-bytes/102400"
            :stream #t
            :params '(("chunk_size" . "8192") ("seed" . "3"))
            :callback
            (lambda (chunk)
              (set! chunks (+ chunks 1))
              (set! total-bytes (+ total-bytes (string-length chunk)))
              #t
            ) ;lambda
          ) ;http-get
        ) ;r
       ) ;
    (check-true (hash-table? r))
    (check (r 'status-code) => 200)
    (check-true (> chunks 0))
    (check total-bytes => 102400)
  ) ;let
) ;let

;; 通过 :stream #t + :output-file 下载 1MB 文件到本地，验证最终文件大小
(let* ((output-file "/tmp/goldfish-http-get-large-1m.dat")
       (r (begin
            (when (file-exists? output-file)
              (delete-file output-file)
            ) ;when
            (http-get "https://proof.ovh.net/files/1Mb.dat"
              :stream      #t
              :output-file output-file
            ) ;http-get
          ) ;begin
       ) ;r
      ) ;
  (check-true (hash-table? r))
  (check (r 'status-code) => 200)
  (check-true (file-exists? output-file))
  (check (binary-file-size output-file) => 1048576)
  (delete-file output-file)
) ;let*

;; 下载 1MB 文件时同时落盘并通过回调统计字节数
(let* ((output-file "/tmp/goldfish-http-get-large-1m-stream.dat")
       (callback-bytes 0)
       (chunks 0)
       (r
         (begin
           (when (file-exists? output-file)
             (delete-file output-file)
           ) ;when
           (http-get "https://proof.ovh.net/files/1Mb.dat"
             :stream      #t
             :output-file output-file
             :callback
             (lambda (chunk)
               (set! chunks (+ chunks 1))
               (set! callback-bytes (+ callback-bytes (string-length chunk)))
               #t
             ) ;lambda
           ) ;http-get
         ) ;begin
       ) ;r
      ) ;
  (check-true (hash-table? r))
  (check (r 'status-code) => 200)
  (check-true (> chunks 0))
  (check callback-bytes => 1048576)
  (check-true (file-exists? output-file))
  (check (binary-file-size output-file) => 1048576)
  (delete-file output-file)
) ;let*

(check-report)
