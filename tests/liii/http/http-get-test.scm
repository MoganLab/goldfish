(import (liii check)
        (liii http)
        (liii string)
        (liii os)
        (scheme file)
) ;import

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env) (exit 0))
) ;let

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
                  (+ n (string-length chunk))
            ) ;loop
          ) ;if
        ) ;let
      ) ;lambda
      (lambda ()
        (close-port port)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let
) ;define



;; http-get
;; 发送 HTTP GET 请求，返回响应对象。
;;
;; 语法
;; ----
;; (http-get url :params params :headers headers :proxy proxy
;;           :output-file output-file :stream stream :callback callback)
;;
;; 参数
;; ----
;; url     : string          - 请求的目标 URL
;; params  : alist (可选)    - 查询参数列表，如 '(("key" . "value"))
;; headers : alist (可选)    - 请求头列表
;; proxy      : alist (可选)    - 代理配置
;; output-file: string (可选)   - stream 模式下将响应体按 chunk 写入本地文件
;; stream     : boolean (可选)  - 启用简单流式模式
;; callback   : procedure (可选)- 按 chunk 处理响应体 (lambda (chunk) ...)
;;
;; 返回值
;; ----
;; stream 为 #f 时返回 response-object；
;; stream 为 #t 时返回 #<undefined>。
;;
;; 描述
;; ----
;; GET 请求用于从服务器获取资源。查询参数会自动编码并附加到 URL。
;; 当 :stream 为 #t 时，会在 Scheme 层按 chunk 处理响应体。
;; 这个简单流式模式不再返回 response-object。

;; 基本 GET 请求
(let ((r (http-get "https://httpbin.org")))
  (check (r 'status-code) => 200)
  (check-true (> (string-length (r 'text)) 0))
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8")
) ;let

;; 带查询参数的 GET 请求
(let
  ((r
     (http-get "https://httpbin.org/get"
              :params '(("key1" . "value1") ("key2" . "value2")))
     ) ;http-get
   ) ;r
  (check-true (string-contains (r 'text) "value1"))
  (check-true (string-contains (r 'text) "value2"))
  (check (r 'url) => "https://httpbin.org/get?key1=value1&key2=value2")
) ;let

;; 通过 :stream #t + :callback 流式处理响应体
(let ((collected '()))
  (let
    ((r
       (http-get "https://httpbin.org/get"
                 :stream #t
                 :params '(("download" . "stream"))
                 :callback (lambda (chunk)
                             (when (> (string-length chunk) 0)
                               (set! collected (cons chunk collected))
                             ) ;when
                             #t
                           ) ;lambda
       ) ;
       ) ;http-get
     ) ;r
    (check-true (undefined? r))
    (check-true (> (length collected) 0))
    (let ((body (string-join (reverse collected) "")))
      (check-true (string-contains body "stream"))
    ) ;let
  ) ;let
) ;let

;; 通过 :stream #t + :headers 流式处理响应体
(let ((collected '()))
  (let
    ((r
       (http-get "https://httpbin.org/headers"
                 :stream #t
                 :headers '(("X-Goldfish-Test" . "stream-get"))
                 :callback (lambda (chunk)
                             (when (> (string-length chunk) 0)
                               (set! collected (cons chunk collected))
                             ) ;when
                             #t
                           ) ;lambda
       ) ;
       ) ;http-get
     ) ;r
    (check-true (undefined? r))
    (check-true (> (length collected) 0))
    (let ((body (string-join (reverse collected) "")))
      (check-true (string-contains body "X-Goldfish-Test"))
      (check-true (string-contains body "stream-get"))
    ) ;let
  ) ;let
) ;let

;; 通过 :stream #t + :output-file 将响应体按 chunk 写入本地文件
(let*
  ((output-file "/tmp/goldfish-http-get-download.txt")
   (r (begin
        (when (file-exists? output-file)
          (delete-file output-file)
        ) ;when
        (http-get "https://jsonplaceholder.typicode.com/posts/1"
                  :stream #t
                  :output-file output-file
        ) ;http-get
      ) ;begin
   ) ;r
  ) ;
  (check-true (undefined? r))
  (check-true (file-exists? output-file))
  (let
    ((body
       (call-with-input-file output-file
         (lambda (port)
           (read-string 4096 port)
         ) ;lambda
       ) ;call-with-input-file
     ) ;body
    ) ;
    (check-true (string-contains body "\"userId\""))
  ) ;let
  (delete-file output-file)
) ;let*

;; 通过 :stream #t + :callback 下载较大的流式响应（100KB），验证 chunk 累计长度
(let ((total-bytes 0)
      (chunks 0))
  (let
    ((r
       (http-get "https://httpbin.org/stream-bytes/102400"
                 :stream #t
                 :params '(("chunk_size" . "8192") ("seed" . "3"))
                 :callback (lambda (chunk)
                             (set! chunks (+ chunks 1))
                             (set! total-bytes (+ total-bytes (string-length chunk)))
                             #t
                           ) ;lambda
       ) ;
       ) ;http-get
     ) ;r
    (check-true (undefined? r))
    (check-true (> chunks 0))
    (check total-bytes => 102400)
  ) ;let
) ;let

;; 通过 :stream #t + :output-file 下载 1MB 文件到本地，验证最终文件大小
(let*
  ((output-file "/tmp/goldfish-http-get-large-1m.dat")
   (r (begin
        (when (file-exists? output-file)
          (delete-file output-file)
        ) ;when
        (http-get "https://proof.ovh.net/files/1Mb.dat"
                  :stream #t
                  :output-file output-file
        ) ;http-get
      ) ;begin
   ) ;r
  ) ;
  (check-true (undefined? r))
  (check-true (file-exists? output-file))
  (check (binary-file-size output-file) => 1048576)
  (delete-file output-file)
) ;let*

;; 下载 1MB 文件时同时落盘并通过回调统计字节数
(let*
  ((output-file "/tmp/goldfish-http-get-large-1m-stream.dat")
   (callback-bytes 0)
   (chunks 0)
   (r (begin
        (when (file-exists? output-file)
          (delete-file output-file)
        ) ;when
        (http-get "https://proof.ovh.net/files/1Mb.dat"
                  :stream #t
                  :output-file output-file
                  :callback (lambda (chunk)
                              (set! chunks (+ chunks 1))
                              (set! callback-bytes (+ callback-bytes (string-length chunk)))
                              #t
                            ) ;lambda
        ) ;http-get
      ) ;begin
   ) ;r
  ) ;
  (check-true (undefined? r))
  (check-true (> chunks 0))
  (check callback-bytes => 1048576)
  (check-true (file-exists? output-file))
  (check (binary-file-size output-file) => 1048576)
  (delete-file output-file)
) ;let*

(check-report)
