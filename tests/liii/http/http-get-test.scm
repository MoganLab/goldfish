(import (liii check)
        (liii http)
        (liii string)
        (liii os)
        (scheme file)
) ;import

(check-set-mode! 'report-failed)

(define (binary-file-size path)
  (let ((port (open-binary-input-file path)))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let loop ((chunk (read-string 8192 port)) (n 0))
          (if (eof-object? chunk)
            n
            (loop (read-string 8192 port)
                  (+ n (string-length chunk)))
          ) ;if
        ) ;let loop
      ) ;lambda
      (lambda ()
        (close-port port)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let
) ;define

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env) (exit 0))
) ;let

;; http-get
;; 发送 HTTP GET 请求，返回响应对象。
;;
;; 语法
;; ----
;; (http-get url :params params :headers headers :proxy proxy
;;           :output-file output-file :stream stream
;;           :callback callback :userdata userdata)
;;
;; 参数
;; ----
;; url     : string          - 请求的目标 URL
;; params  : alist (可选)    - 查询参数列表，如 '(("key" . "value"))
;; headers : alist (可选)    - 请求头列表
;; proxy      : alist (可选)    - 代理配置
;; output-file: string (可选)   - 将响应体直接写入本地文件
;; stream     : boolean (可选)  - 启用流式响应处理
;; callback   : procedure (可选)- 流式处理回调 (lambda (chunk userdata) ...)
;; userdata   : any (可选)      - 传递给 callback 的用户数据
;;
;; 返回值
;; ----
;; response-object
;;   响应对象，可通过 'status-code、'text、'headers 等字段访问。
;;
;; 描述
;; ----
;; GET 请求用于从服务器获取资源。查询参数会自动编码并附加到 URL。
;; 当设置 :output-file 或 (:stream #t :callback ...) 时，会改为边接收边处理响应体。

;; 基本 GET 请求
(let ((r (http-get "https://httpbin.org")))
  (check (r 'status-code) => 200)
  (check-true (> (string-length (r 'text)) 0))
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8")
) ;let

;; 带查询参数的 GET 请求
(let ((r (http-get "https://httpbin.org/get"
                  :params '(("key1" . "value1") ("key2" . "value2")))))
  (check-true (string-contains (r 'text) "value1"))
  (check-true (string-contains (r 'text) "value2"))
  (check (r 'url) => "https://httpbin.org/get?key1=value1&key2=value2")
) ;let

;; 流式 GET，请求返回响应对象，同时通过回调处理 chunk
(let ((collected '()))
  (let ((r (http-get "https://httpbin.org/get"
                     :stream #t
                     :params '(("download" . "stream"))
                     :callback (lambda (chunk userdata)
                                 (when (> (string-length chunk) 0)
                                   (set! collected (cons chunk collected))
                                 ) ;when
                                 #t
                               ) ;lambda
           )))
    (check (r 'status-code) => 200)
    (check-true (> (length collected) 0))
    (let ((body (string-join (reverse collected) "")))
      (check-true (string-contains body "stream"))
    ) ;let
  ) ;let
) ;let

;; 直接将响应体下载到本地文件
(let* ((output-file "/tmp/goldfish-http-get-download.txt")
       (r (begin
            (when (file-exists? output-file)
              (delete-file output-file)
            ) ;when
            (http-get "https://jsonplaceholder.typicode.com/posts/1"
                      :output-file output-file)
          ) ;begin
       ))
  (check (r 'status-code) => 200)
  (check-true (file-exists? output-file))
  (let ((body (call-with-input-file output-file
                (lambda (port)
                  (read-string 4096 port)
                ) ;lambda
              ) ;call-with-input-file
        ))
    (check-true (string-contains body "\"userId\""))
  ) ;let
  (delete-file output-file)
) ;let*

;; 下载较大的流式响应（100KB），验证 chunk 累计长度
(let ((total-bytes 0)
      (chunks 0))
  (let ((r (http-get "https://httpbin.org/stream-bytes/102400"
                     :stream #t
                     :params '(("chunk_size" . "8192") ("seed" . "3"))
                     :callback (lambda (chunk userdata)
                                 (set! chunks (+ chunks 1))
                                 (set! total-bytes (+ total-bytes (string-length chunk)))
                                 #t
                               ) ;lambda
           )))
    (check (r 'status-code) => 200)
    (check-true (> chunks 0))
    (check total-bytes => 102400)
    (check (string-length (r 'text)) => 0)
  ) ;let
) ;let

;; 下载 1MB 文件到本地，验证最终文件大小
(let* ((output-file "/tmp/goldfish-http-get-large-1m.dat")
       (r (begin
            (when (file-exists? output-file)
              (delete-file output-file)
            ) ;when
            (http-get "https://proof.ovh.net/files/1Mb.dat"
                      :output-file output-file)
          ) ;begin
       ))
  (check (r 'status-code) => 200)
  (check-true (file-exists? output-file))
  (check (binary-file-size output-file) => 1048576)
  (delete-file output-file)
) ;let*

;; 下载 1MB 文件时同时落盘并通过回调统计字节数
(let* ((output-file "/tmp/goldfish-http-get-large-1m-stream.dat")
       (callback-bytes 0)
       (chunks 0)
       (r (begin
            (when (file-exists? output-file)
              (delete-file output-file)
            ) ;when
            (http-get "https://proof.ovh.net/files/1Mb.dat"
                      :stream #t
                      :output-file output-file
                      :callback (lambda (chunk userdata)
                                  (set! chunks (+ chunks 1))
                                  (set! callback-bytes (+ callback-bytes (string-length chunk)))
                                  #t
                                ) ;lambda
            ) ;http-get
          ) ;begin
       ))
  (check (r 'status-code) => 200)
  (check-true (> chunks 0))
  (check callback-bytes => 1048576)
  (check-true (file-exists? output-file))
  (check (binary-file-size output-file) => 1048576)
  (delete-file output-file)
) ;let*

(check-report)
